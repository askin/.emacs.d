# Installation
```bash
cd ~
git clone git@github.com:askin/.emacs.d.git
```

After all packages installed, install latest fonts.

    M-x all-the-icons-install-fonts

# Emacs Tips
* Rectangle Select `C-x space`
* Create gist from current buffer `C-c b`
* Open region with default browser `C-c u`
* Run elisp code `C-x C-e`
* Goto line `C-c g`
* Comment region or block `C-c c`

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
