# -*- coding: utf-8 -*-
# TODO:
#  - layout of widgets
#    - sprite-sheet choice button
#    - image preview
#    - frame dimensions
#    - slider fps/ms
#    - animation area
#    - reload / play/pause / quit buttons
#


import gobject
import gtk
from gtk import gdk

frames = []
playing_p = False
cur_frame = 0
timestamp = gobject.get_current_time()
path = ''

def main():
    w = gtk.Window()
    w.set_position(gtk.WIN_POS_CENTER)
    w.connect('destroy', lambda w:gtk.main_quit())
    vbox = gtk.VBox(False, 5)
    hbox = gtk.HBox(False, 5)
    vbox.pack_start(hbox)
    frame = gtk.Frame('Animation')
    animation = gtk.Image()
    animation.set_from_stock(gtk.STOCK_MISSING_IMAGE, gtk.ICON_SIZE_DIALOG)
    frame.add(animation)
    hbox.pack_start(frame, expand=False)
    hbox.pack_start(gtk.VSeparator())
    frame = gtk.Frame('Sprite Sheet')
    sprite_preview = gtk.Image()
    sprite_preview.set_from_stock(gtk.STOCK_MISSING_IMAGE, gtk.ICON_SIZE_DIALOG)
    frame.add(sprite_preview)
    hbox.pack_start(frame, expand=False)

    hbox = gtk.HBox(False, 5)
    hbox.pack_start(gtk.Label('Frame dimensions:'), expand=False)
    frame_w = gtk.Adjustment(value=16, lower=1, upper=256, step_incr=1)
    frame_h = gtk.Adjustment(value=16, lower=1, upper=256, step_incr=1)
    hbox.pack_start(gtk.SpinButton(frame_w), expand=False)
    hbox.pack_start(gtk.Label('x'), expand=False)
    hbox.pack_start(gtk.SpinButton(frame_h), expand=False)
    n_frames_lbl = gtk.Label('')
    hbox.pack_end(n_frames_lbl, expand=False)
    vbox.pack_start(hbox)

    vbox.pack_start(gtk.Label("Don't forget to refresh if you change frame dimensions!"), expand=False)

    hbox = gtk.HBox(False, 5)
    hbox.pack_start(gtk.Label('Frame range:'), expand=False)
    frame_start = gtk.Adjustment(value=0, lower=0, upper=255, step_incr=1)
    frame_end = gtk.Adjustment(value=0, lower=0, upper=255, step_incr=1)
    def on_start_changed(adj):
        if frame_start.get_value() > frame_end.get_value():
            frame_end.set_value(frame_start.get_value())
    frame_start.connect('value-changed', on_start_changed)
    frame_end.connect('value-changed', on_start_changed)
    hbox.pack_start(gtk.SpinButton(frame_start), expand=False)
    hbox.pack_start(gtk.Label(u'→'), expand=False)
    hbox.pack_start(gtk.SpinButton(frame_end), expand=False)
    vbox.pack_start(hbox)

    hbox = gtk.HBox(False, 5)
    hbox.pack_start(gtk.Label('Animation speed (ms):'), expand=False)
    anim_speed = gtk.Adjustment(value = 30, lower=0, upper=1000, step_incr=5)
    scale = gtk.HScale(anim_speed)
    hbox.pack_start(scale)
    hbox.pack_start(gtk.SpinButton(anim_speed), expand=False)
    vbox.pack_start(hbox)

    buttons = gtk.HButtonBox()
    vbox.pack_end(buttons, expand=False)
    file_chooser = gtk.FileChooserButton('Choose a sprite sheet')
    buttons.add(file_chooser)
    reload_btn = gtk.Button('Reload', gtk.STOCK_REFRESH)
    buttons.add(reload_btn)
    play_btn = gtk.Button('Play', gtk.STOCK_MEDIA_PLAY)
    buttons.add(play_btn)
    for b in reload_btn, play_btn: b.set_sensitive(False)
    b = gtk.Button('Quit', gtk.STOCK_QUIT)
    b.connect('clicked', lambda w:gtk.main_quit())
    buttons.add(b)

    def idle_loop():
        global timestamp, cur_frame
        if not playing_p: return False
        new_ts = gobject.get_current_time()
        if (new_ts - timestamp)*1000 < anim_speed.get_value(): return True
        timestamp = new_ts
        (fs,fe) = map(lambda x:int(x.get_value()), (frame_start,frame_end))
        cur_frame = cur_frame + 1
        if cur_frame > fe or cur_frame < fs: cur_frame = fs
        animation.set_from_pixbuf(frames[cur_frame])
        return True
    def on_play(btn):
        global playing_p
        playing_p = not playing_p
        btn.set_label(gtk.STOCK_MEDIA_PAUSE if playing_p else gtk.STOCK_MEDIA_PLAY)
        if playing_p:
            gobject.idle_add(idle_loop)
    play_btn.connect('clicked', on_play)

    def repopulate_frames(pixbuf):
        global frames
        w,h = pixbuf.get_width(), pixbuf.get_height()
        fw,fh = int(frame_w.get_value()), int(frame_h.get_value())
        frames = []
        for y in range(0, h, fh):
            for x in range(0, w, fw):
                frames.append(pixbuf.subpixbuf(x,y,fw,fh))

    def load_anim():
        global cur_frame, playing_p
        successful = False
        try:
            pixbuf = gdk.pixbuf_new_from_file(path)
            sprite_preview.set_from_pixbuf(pixbuf)
            n_frames = int((pixbuf.get_width()//frame_w.get_value())*(pixbuf.get_height()//frame_h.get_value()))
            for adj in (frame_start, frame_end): adj.set_upper(n_frames-1)
            n_frames_lbl.set_text("%d frames" % n_frames)
            repopulate_frames(pixbuf)
            cur_frame = int(frame_start.get_value())
            animation.set_from_pixbuf(frames[cur_frame])
            successful = True
        except Exception, e: print e
        playing_p = False
        play_btn.set_label(gtk.STOCK_MEDIA_PAUSE if playing_p else gtk.STOCK_MEDIA_PLAY)
        for b in reload_btn, play_btn: b.set_sensitive(successful)

    def on_refresh(w): load_anim()
    reload_btn.connect('clicked', on_refresh)

    def on_file_set(chooser):
        global path
        path = chooser.get_filename()
        frame.set_label(path)
        load_anim()
    file_chooser.connect('file-set', on_file_set)

    w.add(vbox)
    w.show_all()
    gtk.main()

if __name__ == "__main__": main()
