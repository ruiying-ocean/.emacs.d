/*
 * pdfkit-reader-module.m --- Minimal native PDFKit bridge for Emacs
 *
 * Copyright (C) 2026 Rui Ying
 * Copyright (C) 2026 Chao Huang
 *
 * Author: Rui Ying <r.ying@uea.ac.uk>
 *
 * The native-view embedding approach is derived in part from Appine:
 * https://github.com/chaoswork/appine
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#import <Cocoa/Cocoa.h>
#import <PDFKit/PDFKit.h>

#include <emacs-module.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

int plugin_is_GPL_compatible;

@interface PDFKitReaderController : NSObject

@property(nonatomic, copy) NSString *identifier;
@property(nonatomic, copy) NSString *path;
@property(nonatomic, strong) NSView *containerView;
@property(nonatomic, strong) PDFView *pdfView;
@property(nonatomic, strong) PDFDocument *document;
@property(nonatomic, weak) NSWindow *hostWindow;
@property(nonatomic, weak) NSResponder *emacsResponder;
@property(nonatomic, strong) id mouseMonitor;
@property(nonatomic) BOOL visible;

- (instancetype)initWithIdentifier:(NSString *)identifier
                              path:(NSString *)path;
- (BOOL)attachToWindow:(NSWindow *)window rect:(NSRect)rect;
- (void)setReaderVisible:(BOOL)visible;
- (BOOL)performAction:(NSString *)action;
- (BOOL)goToPageIndex:(NSInteger)index;
- (NSArray<NSNumber *> *)pageInfo;
- (void)close;

@end

static NSMutableDictionary<NSString *, PDFKitReaderController *> *
    PDFKitReaderControllers(void) {
  static NSMutableDictionary<NSString *, PDFKitReaderController *> *controllers;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    controllers = [[NSMutableDictionary alloc] init];
  });
  return controllers;
}

static void PDFKitReaderRunOnMainSync(dispatch_block_t block) {
  if ([NSThread isMainThread]) {
    block();
  } else {
    dispatch_sync(dispatch_get_main_queue(), block);
  }
}

static BOOL PDFKitReaderUsableWindow(NSWindow *window) {
  return window != nil && window.contentView != nil && window.isVisible &&
         ![window isKindOfClass:[NSPanel class]];
}

static NSWindow *PDFKitReaderTargetWindow(void) {
  NSWindow *window = NSApp.keyWindow;
  if (PDFKitReaderUsableWindow(window)) {
    return window;
  }

  window = NSApp.mainWindow;
  if (PDFKitReaderUsableWindow(window)) {
    return window;
  }

  for (NSWindow *candidate in NSApp.orderedWindows) {
    if (PDFKitReaderUsableWindow(candidate)) {
      return candidate;
    }
  }
  return nil;
}

static BOOL PDFKitReaderResponderIsInsideView(NSResponder *responder,
                                                NSView *view) {
  if (![responder isKindOfClass:[NSView class]]) {
    return NO;
  }
  NSView *responderView = (NSView *)responder;
  return responderView == view || [responderView isDescendantOf:view];
}

static NSScrollView *PDFKitReaderFindScrollView(NSView *view) {
  if ([view isKindOfClass:[NSScrollView class]]) {
    return (NSScrollView *)view;
  }
  for (NSView *subview in view.subviews) {
    NSScrollView *found = PDFKitReaderFindScrollView(subview);
    if (found != nil) {
      return found;
    }
  }
  return nil;
}

@implementation PDFKitReaderController

- (instancetype)initWithIdentifier:(NSString *)identifier
                              path:(NSString *)path {
  self = [super init];
  if (self == nil) {
    return nil;
  }

  NSURL *url = [NSURL fileURLWithPath:path];
  PDFDocument *document = [[PDFDocument alloc] initWithURL:url];
  if (document == nil || document.pageCount == 0) {
    return nil;
  }

  _identifier = [identifier copy];
  _path = [path copy];
  _document = document;

  _containerView = [[NSView alloc] initWithFrame:NSZeroRect];
  _containerView.autoresizesSubviews = YES;
  _containerView.hidden = YES;

  _pdfView = [[PDFView alloc] initWithFrame:NSZeroRect];
  _pdfView.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
  _pdfView.displayMode = kPDFDisplaySinglePageContinuous;
  _pdfView.displayDirection = kPDFDisplayDirectionVertical;
  _pdfView.displayBox = kPDFDisplayBoxCropBox;
  _pdfView.displaysPageBreaks = YES;
  _pdfView.pageBreakMargins = NSEdgeInsetsMake(8.0, 8.0, 8.0, 8.0);
  _pdfView.pageShadowsEnabled = NO;
  _pdfView.backgroundColor = NSColor.controlBackgroundColor;
  _pdfView.autoScales = YES;
  _pdfView.document = document;
  [_containerView addSubview:_pdfView];

  /*
   * PDFView normally becomes first responder after a click.  Restore the
   * Emacs responder after mouse-up so Emacs keymaps keep working, while
   * leaving native scrolling and text selection in PDFKit.
   */
  __weak PDFKitReaderController *weakSelf = self;
  _mouseMonitor = [NSEvent
      addLocalMonitorForEventsMatchingMask:(NSEventMaskLeftMouseUp |
                                            NSEventMaskRightMouseUp |
                                            NSEventMaskOtherMouseUp)
                                   handler:^NSEvent *(NSEvent *event) {
    PDFKitReaderController *strongSelf = weakSelf;
    if (strongSelf == nil || !strongSelf.visible ||
        event.window != strongSelf.hostWindow) {
      return event;
    }

    NSPoint point = [strongSelf.containerView
        convertPoint:event.locationInWindow
            fromView:nil];
    if (!NSPointInRect(point, strongSelf.containerView.bounds)) {
      return event;
    }

    __weak PDFKitReaderController *deferredSelf = strongSelf;
    dispatch_async(dispatch_get_main_queue(), ^{
      PDFKitReaderController *controller = deferredSelf;
      NSWindow *host = controller.hostWindow;
      NSResponder *responder = controller.emacsResponder;
      if (controller != nil && controller.visible && host != nil &&
          responder != nil) {
        [host makeFirstResponder:responder];
      }
    });
    return event;
  }];

  return self;
}

- (BOOL)attachToWindow:(NSWindow *)window rect:(NSRect)rect {
  if (window == nil || window.contentView == nil ||
      rect.size.width <= 0.0 || rect.size.height <= 0.0) {
    return NO;
  }

  if (self.hostWindow != window ||
      self.containerView.superview != window.contentView) {
    [self.containerView removeFromSuperview];
    self.hostWindow = window;
    [window.contentView addSubview:self.containerView
                       positioned:NSWindowAbove
                       relativeTo:nil];
  }

  NSResponder *currentResponder = window.firstResponder;
  if (currentResponder != nil &&
      !PDFKitReaderResponderIsInsideView(currentResponder,
                                         self.containerView)) {
    self.emacsResponder = currentResponder;
  }

  PDFPage *currentPage = self.pdfView.currentPage;
  self.containerView.frame = rect;
  self.pdfView.frame = self.containerView.bounds;
  if (currentPage != nil) {
    [self.pdfView goToPage:currentPage];
  }
  self.containerView.hidden = !self.visible;
  return YES;
}

- (void)setReaderVisible:(BOOL)visible {
  self.visible = visible;
  self.containerView.hidden = !visible;
}

- (CGFloat)clampedScale:(CGFloat)scale {
  CGFloat minimum = self.pdfView.minScaleFactor;
  CGFloat maximum = self.pdfView.maxScaleFactor;
  if (minimum > 0.0) {
    scale = scale < minimum ? minimum : scale;
  }
  if (maximum > 0.0) {
    scale = scale > maximum ? maximum : scale;
  }
  return scale;
}

- (BOOL)fitWidth {
  PDFPage *page = self.pdfView.currentPage;
  if (page == nil) {
    page = [self.document pageAtIndex:0];
  }
  if (page == nil || self.pdfView.bounds.size.width <= 0.0) {
    return NO;
  }

  NSRect pageBounds = [page boundsForBox:self.pdfView.displayBox];
  NSInteger rotation = ((page.rotation % 360) + 360) % 360;
  CGFloat pageWidth =
      (rotation == 90 || rotation == 270) ? pageBounds.size.height
                                          : pageBounds.size.width;
  if (pageWidth <= 0.0) {
    return NO;
  }

  CGFloat availableWidth = self.pdfView.bounds.size.width - 24.0;
  availableWidth = availableWidth < 1.0 ? 1.0 : availableWidth;
  self.pdfView.autoScales = NO;
  self.pdfView.scaleFactor =
      [self clampedScale:(availableWidth / pageWidth)];
  [self.pdfView goToPage:page];
  return YES;
}

- (BOOL)sendScrollAction:(SEL)selector {
  NSScrollView *scrollView = PDFKitReaderFindScrollView(self.pdfView);
  if (scrollView == nil || ![scrollView respondsToSelector:selector]) {
    return NO;
  }
  return [NSApp sendAction:selector to:scrollView from:nil];
}

- (BOOL)performAction:(NSString *)action {
  if ([action isEqualToString:@"next-page"]) {
    [self.pdfView goToNextPage:nil];
  } else if ([action isEqualToString:@"previous-page"]) {
    [self.pdfView goToPreviousPage:nil];
  } else if ([action isEqualToString:@"first-page"]) {
    return [self goToPageIndex:0];
  } else if ([action isEqualToString:@"last-page"]) {
    return [self goToPageIndex:(NSInteger)self.document.pageCount - 1];
  } else if ([action isEqualToString:@"zoom-in"]) {
    PDFPage *page = self.pdfView.currentPage;
    self.pdfView.autoScales = NO;
    self.pdfView.scaleFactor =
        [self clampedScale:(self.pdfView.scaleFactor * 1.2)];
    if (page != nil) {
      [self.pdfView goToPage:page];
    }
  } else if ([action isEqualToString:@"zoom-out"]) {
    PDFPage *page = self.pdfView.currentPage;
    self.pdfView.autoScales = NO;
    self.pdfView.scaleFactor =
        [self clampedScale:(self.pdfView.scaleFactor / 1.2)];
    if (page != nil) {
      [self.pdfView goToPage:page];
    }
  } else if ([action isEqualToString:@"fit-page"]) {
    PDFPage *page = self.pdfView.currentPage;
    self.pdfView.autoScales = YES;
    if (page != nil) {
      [self.pdfView goToPage:page];
    }
  } else if ([action isEqualToString:@"fit-width"]) {
    return [self fitWidth];
  } else if ([action isEqualToString:@"scroll-page-down"]) {
    /* PDFKit's internal document view uses the opposite selector name. */
    return [self sendScrollAction:@selector(scrollPageUp:)];
  } else if ([action isEqualToString:@"scroll-page-up"]) {
    return [self sendScrollAction:@selector(scrollPageDown:)];
  } else if ([action isEqualToString:@"scroll-line-down"]) {
    return [self sendScrollAction:@selector(scrollLineUp:)];
  } else if ([action isEqualToString:@"scroll-line-up"]) {
    return [self sendScrollAction:@selector(scrollLineDown:)];
  } else if ([action isEqualToString:@"top"]) {
    return [self sendScrollAction:@selector(scrollToBeginningOfDocument:)];
  } else if ([action isEqualToString:@"bottom"]) {
    return [self sendScrollAction:@selector(scrollToEndOfDocument:)];
  } else {
    return NO;
  }
  return YES;
}

- (BOOL)goToPageIndex:(NSInteger)index {
  if (index < 0 || index >= (NSInteger)self.document.pageCount) {
    return NO;
  }
  PDFPage *page = [self.document pageAtIndex:(NSUInteger)index];
  if (page == nil) {
    return NO;
  }
  [self.pdfView goToPage:page];
  return YES;
}

- (NSArray<NSNumber *> *)pageInfo {
  PDFPage *page = self.pdfView.currentPage;
  if (page == nil) {
    return nil;
  }
  NSUInteger index = [self.document indexForPage:page];
  if (index == NSNotFound) {
    return nil;
  }
  return @[ @(index + 1), @(self.document.pageCount) ];
}

- (void)close {
  if (self.mouseMonitor != nil) {
    [NSEvent removeMonitor:self.mouseMonitor];
    self.mouseMonitor = nil;
  }
  self.visible = NO;
  self.pdfView.document = nil;
  [self.containerView removeFromSuperview];
  self.hostWindow = nil;
  self.emacsResponder = nil;
}

- (void)dealloc {
  if (_mouseMonitor != nil) {
    [NSEvent removeMonitor:_mouseMonitor];
  }
}

@end

static NSString *PDFKitReaderString(const char *value) {
  if (value == NULL) {
    return nil;
  }
  return [NSString stringWithUTF8String:value];
}

static BOOL PDFKitReaderOpen(const char *identifierCString,
                             const char *pathCString, int x, int y, int width,
                             int height) {
  NSString *identifier = PDFKitReaderString(identifierCString);
  NSString *path = [PDFKitReaderString(pathCString) stringByStandardizingPath];
  if (identifier.length == 0 || path.length == 0) {
    return NO;
  }

  __block BOOL result = NO;
  PDFKitReaderRunOnMainSync(^{
    NSWindow *targetWindow = PDFKitReaderTargetWindow();
    if (targetWindow == nil) {
      return;
    }

    NSMutableDictionary *controllers = PDFKitReaderControllers();
    PDFKitReaderController *controller = controllers[identifier];

    if (controller != nil && ![controller.path isEqualToString:path]) {
      [controller close];
      [controllers removeObjectForKey:identifier];
      controller = nil;
    }

    if (controller == nil) {
      controller =
          [[PDFKitReaderController alloc] initWithIdentifier:identifier
                                                       path:path];
      if (controller == nil) {
        return;
      }
      controllers[identifier] = controller;
    }

    controller.visible = YES;
    NSRect rect = NSMakeRect((CGFloat)x, (CGFloat)y, (CGFloat)width,
                             (CGFloat)height);
    result = [controller attachToWindow:targetWindow rect:rect];
    if (result) {
      [controller setReaderVisible:YES];
    }
  });
  return result;
}

static BOOL PDFKitReaderUpdate(const char *identifierCString, int x, int y,
                               int width, int height) {
  NSString *identifier = PDFKitReaderString(identifierCString);
  if (identifier.length == 0) {
    return NO;
  }

  __block BOOL result = NO;
  PDFKitReaderRunOnMainSync(^{
    PDFKitReaderController *controller =
        PDFKitReaderControllers()[identifier];
    if (controller == nil) {
      return;
    }
    NSRect rect = NSMakeRect((CGFloat)x, (CGFloat)y, (CGFloat)width,
                             (CGFloat)height);
    result = [controller attachToWindow:PDFKitReaderTargetWindow() rect:rect];
  });
  return result;
}

static BOOL PDFKitReaderSetVisible(const char *identifierCString,
                                   BOOL visible) {
  NSString *identifier = PDFKitReaderString(identifierCString);
  if (identifier.length == 0) {
    return NO;
  }

  __block BOOL result = NO;
  PDFKitReaderRunOnMainSync(^{
    PDFKitReaderController *controller =
        PDFKitReaderControllers()[identifier];
    if (controller != nil) {
      [controller setReaderVisible:visible];
      result = YES;
    }
  });
  return result;
}

static BOOL PDFKitReaderAction(const char *identifierCString,
                               const char *actionCString) {
  NSString *identifier = PDFKitReaderString(identifierCString);
  NSString *action = PDFKitReaderString(actionCString);
  if (identifier.length == 0 || action.length == 0) {
    return NO;
  }

  __block BOOL result = NO;
  PDFKitReaderRunOnMainSync(^{
    PDFKitReaderController *controller =
        PDFKitReaderControllers()[identifier];
    result = [controller performAction:action];
  });
  return result;
}

static BOOL PDFKitReaderGoToPage(const char *identifierCString,
                                 NSInteger index) {
  NSString *identifier = PDFKitReaderString(identifierCString);
  if (identifier.length == 0) {
    return NO;
  }

  __block BOOL result = NO;
  PDFKitReaderRunOnMainSync(^{
    PDFKitReaderController *controller =
        PDFKitReaderControllers()[identifier];
    result = [controller goToPageIndex:index];
  });
  return result;
}

static NSArray<NSNumber *> *
PDFKitReaderPageInfo(const char *identifierCString) {
  NSString *identifier = PDFKitReaderString(identifierCString);
  if (identifier.length == 0) {
    return nil;
  }

  __block NSArray<NSNumber *> *result = nil;
  PDFKitReaderRunOnMainSync(^{
    PDFKitReaderController *controller =
        PDFKitReaderControllers()[identifier];
    result = [controller pageInfo];
  });
  return result;
}

static BOOL PDFKitReaderClose(const char *identifierCString) {
  NSString *identifier = PDFKitReaderString(identifierCString);
  if (identifier.length == 0) {
    return NO;
  }

  __block BOOL result = NO;
  PDFKitReaderRunOnMainSync(^{
    NSMutableDictionary *controllers = PDFKitReaderControllers();
    PDFKitReaderController *controller = controllers[identifier];
    if (controller != nil) {
      [controller close];
      [controllers removeObjectForKey:identifier];
      result = YES;
    }
  });
  return result;
}

static void PDFKitReaderCloseAll(void) {
  PDFKitReaderRunOnMainSync(^{
    NSMutableDictionary *controllers = PDFKitReaderControllers();
    for (PDFKitReaderController *controller in controllers.allValues) {
      [controller close];
    }
    [controllers removeAllObjects];
  });
}

static char *PDFKitReaderCopyEmacsString(emacs_env *env, emacs_value value) {
  ptrdiff_t size = 0;
  if (!env->copy_string_contents(env, value, NULL, &size) || size <= 0) {
    return NULL;
  }
  char *buffer = malloc((size_t)size);
  if (buffer == NULL) {
    return NULL;
  }
  if (!env->copy_string_contents(env, value, buffer, &size)) {
    free(buffer);
    return NULL;
  }
  return buffer;
}

static emacs_value PDFKitReaderBool(emacs_env *env, BOOL value) {
  return env->intern(env, value ? "t" : "nil");
}

static int PDFKitReaderInt(emacs_env *env, emacs_value value) {
  return (int)env->extract_integer(env, value);
}

static emacs_value FPDFKitReaderOpen(emacs_env *env, ptrdiff_t nargs,
                                      emacs_value *args, void *data) {
  (void)nargs;
  (void)data;
  char *identifier = PDFKitReaderCopyEmacsString(env, args[0]);
  char *path = PDFKitReaderCopyEmacsString(env, args[1]);
  if (identifier == NULL || path == NULL) {
    free(identifier);
    free(path);
    return env->intern(env, "nil");
  }
  BOOL result = PDFKitReaderOpen(
      identifier, path, PDFKitReaderInt(env, args[2]),
      PDFKitReaderInt(env, args[3]), PDFKitReaderInt(env, args[4]),
      PDFKitReaderInt(env, args[5]));
  free(identifier);
  free(path);
  return PDFKitReaderBool(env, result);
}

static emacs_value FPDFKitReaderUpdate(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value *args, void *data) {
  (void)nargs;
  (void)data;
  char *identifier = PDFKitReaderCopyEmacsString(env, args[0]);
  if (identifier == NULL) {
    return env->intern(env, "nil");
  }
  BOOL result = PDFKitReaderUpdate(
      identifier, PDFKitReaderInt(env, args[1]),
      PDFKitReaderInt(env, args[2]), PDFKitReaderInt(env, args[3]),
      PDFKitReaderInt(env, args[4]));
  free(identifier);
  return PDFKitReaderBool(env, result);
}

static emacs_value FPDFKitReaderSetVisible(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value *args, void *data) {
  (void)nargs;
  (void)data;
  char *identifier = PDFKitReaderCopyEmacsString(env, args[0]);
  if (identifier == NULL) {
    return env->intern(env, "nil");
  }
  BOOL result =
      PDFKitReaderSetVisible(identifier, env->is_not_nil(env, args[1]));
  free(identifier);
  return PDFKitReaderBool(env, result);
}

static emacs_value FPDFKitReaderAction(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value *args, void *data) {
  (void)nargs;
  (void)data;
  char *identifier = PDFKitReaderCopyEmacsString(env, args[0]);
  char *action = PDFKitReaderCopyEmacsString(env, args[1]);
  if (identifier == NULL || action == NULL) {
    free(identifier);
    free(action);
    return env->intern(env, "nil");
  }
  BOOL result = PDFKitReaderAction(identifier, action);
  free(identifier);
  free(action);
  return PDFKitReaderBool(env, result);
}

static emacs_value FPDFKitReaderGoToPage(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value *args, void *data) {
  (void)nargs;
  (void)data;
  char *identifier = PDFKitReaderCopyEmacsString(env, args[0]);
  if (identifier == NULL) {
    return env->intern(env, "nil");
  }
  intmax_t index = env->extract_integer(env, args[1]);
  BOOL result = PDFKitReaderGoToPage(identifier, (NSInteger)index);
  free(identifier);
  return PDFKitReaderBool(env, result);
}

static emacs_value FPDFKitReaderPageInfo(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value *args, void *data) {
  (void)nargs;
  (void)data;
  char *identifier = PDFKitReaderCopyEmacsString(env, args[0]);
  if (identifier == NULL) {
    return env->intern(env, "nil");
  }
  NSArray<NSNumber *> *info = PDFKitReaderPageInfo(identifier);
  free(identifier);
  if (info == nil || info.count != 2) {
    return env->intern(env, "nil");
  }
  emacs_value pair[] = {
      env->make_integer(env, info[0].integerValue),
      env->make_integer(env, info[1].integerValue),
  };
  return env->funcall(env, env->intern(env, "cons"), 2, pair);
}

static emacs_value FPDFKitReaderClose(emacs_env *env, ptrdiff_t nargs,
                                       emacs_value *args, void *data) {
  (void)nargs;
  (void)data;
  char *identifier = PDFKitReaderCopyEmacsString(env, args[0]);
  if (identifier == NULL) {
    return env->intern(env, "nil");
  }
  BOOL result = PDFKitReaderClose(identifier);
  free(identifier);
  return PDFKitReaderBool(env, result);
}

static emacs_value FPDFKitReaderCloseAll(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value *args, void *data) {
  (void)nargs;
  (void)args;
  (void)data;
  PDFKitReaderCloseAll();
  return env->intern(env, "t");
}

typedef emacs_value (*PDFKitReaderEmacsFunction)(emacs_env *, ptrdiff_t,
                                                  emacs_value *, void *);

static void PDFKitReaderBindFunction(emacs_env *env, const char *name,
                                     ptrdiff_t minimumArity,
                                     ptrdiff_t maximumArity,
                                     PDFKitReaderEmacsFunction function,
                                     const char *documentation) {
  emacs_value symbol = env->intern(env, name);
  emacs_value nativeFunction =
      env->make_function(env, minimumArity, maximumArity, function,
                         documentation, NULL);
  emacs_value arguments[] = {symbol, nativeFunction};
  env->funcall(env, env->intern(env, "fset"), 2, arguments);
}

int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);

  PDFKitReaderBindFunction(
      env, "pdfkit-reader-native-open", 6, 6, FPDFKitReaderOpen,
      "Open a PDF for ID at native rect X, Y, WIDTH, HEIGHT.");
  PDFKitReaderBindFunction(
      env, "pdfkit-reader-native-update", 5, 5, FPDFKitReaderUpdate,
      "Move or resize the native PDF view for ID.");
  PDFKitReaderBindFunction(
      env, "pdfkit-reader-native-set-visible", 2, 2,
      FPDFKitReaderSetVisible, "Set the native PDF view for ID visible or hidden.");
  PDFKitReaderBindFunction(
      env, "pdfkit-reader-native-action", 2, 2, FPDFKitReaderAction,
      "Perform ACTION in the native PDF view for ID.");
  PDFKitReaderBindFunction(
      env, "pdfkit-reader-native-goto-page", 2, 2,
      FPDFKitReaderGoToPage, "Go to zero-based page INDEX in the PDF for ID.");
  PDFKitReaderBindFunction(
      env, "pdfkit-reader-native-page-info", 1, 1,
      FPDFKitReaderPageInfo, "Return (CURRENT-PAGE . PAGE-COUNT) for ID.");
  PDFKitReaderBindFunction(
      env, "pdfkit-reader-native-close", 1, 1, FPDFKitReaderClose,
      "Close the native PDF view for ID.");
  PDFKitReaderBindFunction(
      env, "pdfkit-reader-native-close-all", 0, 0, FPDFKitReaderCloseAll,
      "Close every native PDF view.");

  emacs_value feature = env->intern(env, "pdfkit-reader-module");
  emacs_value provideArguments[] = {feature};
  env->funcall(env, env->intern(env, "provide"), 1, provideArguments);
  return 0;
}
