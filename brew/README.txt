
brew install --cask qlcolorcode

brew install highlight

xattr -d -r com.apple.quarantine ~/Library/QuickLook/QLColorCode.qlgenerator

add `file_UTI` to the LSItemContentTypes section in the file `~/Library/QuickLook/QLColorCode.qlgenerator/Contents/Info.plist` where `file_UTI` is obtained via `mdls -name kMDItemContentType filaneme.ext` (see [here](https://github.com/anthonygelibert/QLColorCode/issues/51#issuecomment-566155289))
