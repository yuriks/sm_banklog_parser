math pri on

!SPRM_LARGE = 1<<0

function SPRM_PAL(i) = (i&7)<<9
function SPRM_PRIO(i) = (i&3)<<12
!SPRM_XFLIP = 1<<14
!SPRM_YFLIP = 1<<15

!SPRM_ALL_ATTRIBS = !SPRM_LARGE|SPRM_PAL(7)|SPRM_PRIO(3)|!SPRM_XFLIP|!SPRM_YFLIP

macro spritemap_entry(x, y, i_tile, attribs)
{
    assert <x> < $200, "spritemap entry error: bad X offset"
    assert <y> < $100, "spritemap entry error: bad Y offset"
    assert <i_tile> < $200, "spritemap entry error: bad tile index"
    assert <attribs>&~!SPRM_ALL_ATTRIBS != 0, "spritemap entry error: bad attributes"

    dw <x>|select(<attribs>&!SPRM_LARGE, 1<<15, 0)
    db <y>
    dw <i_tile>|(<attribs>&~!SPRM_LARGE)
}
endmacro

; ex: %spritemap_entry(0, 0, $107, SPRM_PRIO(3))