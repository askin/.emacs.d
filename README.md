# My Personel Emacs Configuration

[![Build Status](https://travis-ci.org/askin/.emacs.d.svg?branch=master)](https://travis-ci.org/askin/.emacs.d)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

# Installation
```bash
cd ~
git clone git@github.com:askin/.emacs.d.git
```

After all packages installed, install latest fonts.

    M-x all-the-icons-install-fonts

# Run as a daemon
Copy `emacs.service` to `~/.config/systemd/user/`

```bash
cp emacs.service ~/.config/systemd/user/
```

Reload units
```bash
systemctl daemon-reload --user
```

Enable emacs service to run startup
```bash
systemctl enable --user emacs
```

Start or stop emacs
```bash
systemctl start --user emacs
systemctl stop --user emacs
```

Start emacs client on gui
```
emacsclient -c
```

Start emacs client on cli
```
emacsclient -t
```

# Emacs Tips
* Rectangle Select `C-x space`
* Create gist from current buffer `C-c b`
* Open region with default browser `C-c u`
* Run elisp code `C-x C-e`
* Goto line `M-g M-g`
* Comment region or block `M-;`
* Start defining macro `<F3>` or `C-x (`
* End defining macro `<F4>` or `C-x )`
  * Examle usage `<F3> M-f foo <F4>`
* Open remote file or directory `C-x C-f` `/ssh:remote-machine:~`
* Open remote file or directory as root `C-x C-f` `/ssh:remote-machine|sudo:root@remote-machine:~`
* Add new line while replace-string `C-q C-j`
  * `C-q`: `M-x quoted-insert`
  * `C-j`: New line
* Rename multiple files
  * Enter dired `C-x d` or `C-x f`
  * Switch to edit mode `C-x C-q`
  * After editing save with `C-c C-c`
* To Upper/Lower case region
  * We have to enable before use
  ```elisp
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  ```
  * To upper `C-x C-u`
  * To lower `C-x C-l`
* Insert a link to document `C-c C-l`
* Repeating a Command [details...](https://www.gnu.org/software/emacs/manual/html_node/emacs/Repeating.html "More Details...")
  * Repeat last command `C-x z`
  * Repeat n-times (10-times for example) `C-u 1 0 command`
    * Delete 10 character `C-u 1 0 C-d`
* Select python virtual env `M-x pyvenv-workon`
* Select all buffer `C-x h`
* Json pretty print
  * Buffer: `M-x json-pretty-print-buffer`
  * Region: `M-x json-pretty-print`
* Create temporary file or directory
  * Create file with extension `M-x create-temp-file`
  * Create directory `M-x create-temp-directory`
* Insert or align comment `M-;` or `M-x indent-for-comment`.

# Key bindings for eyebrowse-mode
The default key bindings are:

| Key bind    | Function                         |
|-------------|----------------------------------|
| `C-c C-w <` | Switch to previous window config |
| `C-c C-w >` | Switch to next window config     |
| `C-c C-w '` | Switch to last window config     |
| `C-c C-w "` | Close current window config      |
| `C-c C-w ,` | Rename current window config     |
| `C-c C-w 0` | Switch to window config `0`      |
| ...         | ...                              |
| `C-c C-w 9` | Switch to window config `9`      |

# Emacs Bookmark

| Key bind                 | Description                                                        |
|--------------------------|--------------------------------------------------------------------|
| `C-x r m <RET>`          | Set the bookmark for the visited file, at point.                   |
| `C-x r m bookmark <RET>` | Set the bookmark named bookmark at point (bookmark-set).           |
| `C-x r M bookmark <RET>` | Like C-x r m, but don't overwrite an existing bookmark.            |
| `C-x r b bookmark <RET>` | Jump to the bookmark named bookmark (bookmark-jump).               |
| `C-x r l`                | List all bookmarks (list-bookmarks).                               |
| `M-x bookmark-save`      | Save all the current bookmark values in the default bookmark file. |
