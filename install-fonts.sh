#!/bin/sh

#--------------------------------------------
#This script installs coding fonts for Emacs
#--------------------------------------------
#To run this script (No need to run by sudo!)
#chmod +rx install-fonts.sh
#bash ./install-fonts.sh
#--------------------------------------------

if [[ "$OSTYPE" == "msys" ]]; then
    echo ">>> Using windows, not sure whether works"
elif [[ "$OSTYPE" == "cygwin" ]]; then
    echo ">>> Using windows, not sure whether works"
else
    echo ">>> Using Linux/OSX, should work well"
fi

#--Font list--
#>English Monospace
#Iosevka
#Inconsolata
#Roboto Mono
#Juliamono
#Fira Code
#Jetbrains Mono
#Hack
#Dejavu
#Ubuntu Mono Nerd

#>CJK mono font (sans font)
#WenQuanYi Micro Hei
#Sarasa Mono SC Nerd
#Noto Sans SC (also Source Sans SC)

#>Variable pitch font (serif font)
#Symbola
#New York
#Cormorant Garamond
#ET Book
#---End----

font_dir="$HOME/.local/share/fonts"

if [ ! -d $font_dir ]
then
    mkdir $font_dir;
fi

cd $font_dir

dejavu_url="https://github.com/dejavu-fonts/dejavu-fonts/releases/download/version_2_37/dejavu-fonts-ttf-2.37.zip"
hack_url="https://github.com/source-foundry/Hack/releases/download/v3.003/Hack-v3.003-ttf.zip"
symbola_url="https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip"
iosevka_url="https://github.com/be5invis/Iosevka/releases/download/v7.2.0/ttf-iosevka-7.2.0.zip"
inconsolata_url="https://github.com/google/fonts/raw/main/ofl/inconsolata/static/Inconsolata-Regular.ttf"
jetbrainmono_url="https://github.com/google/fonts/raw/main/ofl/jetbrainsmono/static/JetBrainsMono-Regular.ttf"
firacode_url="https://github.com/google/fonts/raw/main/ofl/firacode/static/FiraCode-Regular.ttf"
robotomono_url="https://github.com/google/fonts/raw/main/apache/robotomono/static/RobotoMono-Regular.ttf"
juliamono_url="https://github.com/cormullion/juliamono/releases/download/v0.040/JuliaMono-ttf.zip"
newyork_rul="https://github.com/auranticus/New-York-fonts/raw/master/TTF%20version/new-york-medium_regular.ttf"
sarasa_url="https://github.com/laishulu/Sarasa-Mono-SC-Nerd/raw/master/sarasa-mono-sc-nerd-regular.ttf"
wqy_url="https://github.com/anthonyfok/fonts-wqy-microhei/raw/master/wqy-microhei.ttc"
cormorant_url="https://github.com/google/fonts/raw/main/ofl/cormorantgaramond/CormorantGaramond-Regular.ttf"
notosans_url="https://github.com/googlefonts/noto-cjk/raw/main/Sans/Mono/NotoSansMonoCJKsc-Regular.otf"
monaco_url="https://github.com/todylu/monaco.ttf/raw/master/monaco.ttf"
etbook_url="https://github.com/edwardtufte/et-book/raw/gh-pages/et-book/et-book-roman-old-style-figures/et-book-roman-old-style-figures.ttf"
ubuntumono_url="https://github.com/ryanoasis/nerd-fonts/raw/master/patched-fonts/UbuntuMono/Regular/complete/Ubuntu%20Mono%20Nerd%20Font%20Complete%20Mono.ttf"

font_list=($iosevka_url $inconsolata_url $jetbrainmono_url
	   $firacode_url $robotomono_url $symbola_url $juliamono_url
	   $newyork_rul $sarasa_url $wqy_url $cormorant_url
	   $notosans_url $monaco_url $etbook_url $dejavu_url
	   $ubuntumono_url)

echo ">>>> Downloading starts..."
for i in ${font_list[@]}; do
    wget --quiet "$i";
done
echo ">>>> All fonts downloaded :)"

#extract all zip archives
unzip \*.zip

#move all font file out
touch tmp.log
find . -name '*.[ot]t[fc]' -print0 | xargs -0 -I {} mv -vn {} . &> tmp.log

#delete all files other than .otf/.ttf/.ttc
find . -type f ! -name '*.[ot]t[fc]' -delete

#delete all empty folder
find . -type d -empty -delete

#remove the warning log
rm tmp.log

fc-cache -f -v
