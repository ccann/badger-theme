# Badger Color Theme

Medium contrast Emacs 24 color theme (using deftheme). Inspired by the wombat and tomorrow themes.

![alt text](https://raw.github.com/ccann/badger-theme/master/img/badger-theme.png "org-preview")

## org-mode 
Some people might prefer the headline after a DONE keyword to be ~~struck-through~~. Add the following code to your init file:

    (setq org-fontify-done-headline t)

Note that this strikethough extends to the end of the line, so if you tag a headline the strikethrough extends over empty space to reach the tag:

![alt text](https://raw.github.com/ccann/badger-theme/master/img/strikethrough.png "ugly strikethrough")

This doesn't look great, IMO.

If you would prefer the DONE keyword to not have the stikethrough effect use 

    M-x customize-face RET org-done

and change stike-through's value to Off. Save this change so that it applies to future emacs sessions.

