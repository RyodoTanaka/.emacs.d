# My emacs.d
私が使っているemacsの設定です。
- [arduino-mode](http://wikemacs.org/wiki/Arduino-mode)
- [auto-complete](http://cx4a.org/software/auto-complete/index.ja.html)
- [linenumbers](http://www.emacswiki.org/emacs/LineNumbers)
- [markdown-mode](http://jblevins.org/projects/markdown-mode/)
- [emacs-color-themes](https://github.com/owainlewis/emacs-color-themes#contributors)
- [yaml-mode](https://github.com/yoshiki/yaml-mode)
- [Yatex]()
- [auto-complete-latex](http://www.emacswiki.org/emacs/auto-complete-latex.el)
- [elscreen](https://github.com/knu/elscreen)
- [tramp](http://www.emacswiki.org/emacs/TrampMode)
- [Magit](https://github.com/magit/magit)
- [Magit GitFlow]()
- [GnuPlot Mode]()
- [PDF Tools]()

が含まれています。

#####動作環境
OS      :  Ubuntu 14.04 LTS  
Version :  GNU Emacs 24.4.1

##インストール

### Emacs のアップデート
**!! Emacsのバージョンが24.4以上出ないと動きません。!!**  
2015.09.09現在、`apt`でサポートされているのは、Emacs24.3です。  
なので、**手動で最新のEmacsをインストールする**必要があります。注意してください。  
**参考**  
[Ubuntu Handbook](http://ubuntuhandbook.org/index.php/2014/10/emacs-24-4-released-install-in-ubuntu-14-04/)  
[dofmanさんのメモ]()  
1. 不必要なものを予めremoveしておく。
   ubuntu14.04の場合、aptで提供されているemacsのversionは24.3なので
   emacs24.3以下をインストールしている場合はアンインストールする。  
   emacsのversionがわからない場合は、emacsを開いてから次のコマンドを
   実行する。`M-x version`  
   では、emacs24.3と関連パッケージをアンインストールする。以下のコマンドを
   実行する。
   ```bash
   sudo apt-get remove emacs
   sudo apt-get remove emacs24*
   ```
   
2. emacs24.4をソースからビルドし、インストールする。
   1. ビルドに必要な依存パッケージをインストールする。
      ```bash
      sudo apt-get install build-essential
      sudo apt-get build-dep emacs24
      ```

   2. emacs24.4のソースを[公式ページ](http://ftp.gnu.org/gnu/emacs/)からダウンロードする。  
      `emacs-24.4.tar.gz`と書いてある箇所を見つけて、クリックすればダウンロードが始まる。  
      *ここからはダウンロード先が~/Downloads/*以下であると仮定してすすめる。
      ```bash
      cd ~/Downloads
	  wget http://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-24.4.tar.gz
      tar -xf emacs-24.4.tar.gz
      cd emacs-24.4
      ```

   3. 以下のコマンドでemacs24.4をビルドしてインストールする。
      ```bash
	  ./configure
	  make
	  sudo make install
	  ```

### git-2.5.1のinstall
aptで提供されているgitのversionではmagit-gitflow.elが動いてくれないので、
PPAから最新版をインストールする。以下のコマンドを実行すれば良い。
```bash
sudo apt-get remove git
sudo add-apt-repository ppa:git-core/ppa  
sudo apt-get update  
sudo apt-get install git
```
### git-flowのインストール
gitflowをインストールする。
`sudo apt-get install git-flow`

## レポジトリのインストール
ホームディレクトリ直下で、```git clone```を使ってダウンロードしてください。
#####httpsを利用する場合
```
cd ~/
git clone https://github.com/RyodoTanaka/.emacs.d
```
#####sshを利用する場合
```
cd ~/
git clone git@github.com:RyodoTanaka/.emacs.d.git
```

##Memo
`M-x find-name-dired` : ワイルドカードによるファイルの検索結果をdiredバッファに表示 
