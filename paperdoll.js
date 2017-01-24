/* Simple paperdoll-like objects to alter page theme.
 * Julian Squires <julian@cipht.net> / 2011
 */
;(function($){
    function intersect_p(a,b){
        var al = a.offset().left, ar = al+a.width(), at = a.offset().top, ab = at+a.height();
        var bl = b.offset().left, br = bl+b.width(), bt = b.offset().top, bb = bt+b.height();
        return ((at >= bt && at <= bb) || (ab >= bt && ab <= bb) || (at < bt && ab > bb)) &&
            ((al >= bl && al <= br) || (ar >= bl && ar <= br) || (al < bl && ar > br));
    }

    var rotation_style = null;
    function detect_rotation_support() {
        var styles=document.getElementsByTagName("head")[0].style,toCheck="transformProperty WebkitTransform OTransform msTransform MozTransform".split(" ");
        for (var a=0;a<toCheck.length;a++) if (styles[toCheck[a]] !== undefined) { rotation_style = toCheck[a]; break; }
    }

    var animation_interval = 50,
        world_boundary = {top:0, left:0, bottom:600, right:800},
        base_guitar_rotation = 0.523,
        chance_of_heading_change = 0.01,
        chance_of_velocity_change = 0.05,
        damping_factor = 0.05,
        velocity_epsilon = 1;
    function animation_loop() {
        $('.pd.object.drifting').each(function(){
            // ensure we _have_ a heading
            var heading = Number($(this).attr('data_heading') || 4.89),
                velocity = Number($(this).attr('data_velocity') || 20),
                x = Number($(this).attr('data_x') || $(this).offset().left),
                y = Number($(this).attr('data_y') || $(this).offset().top);
            // maybe change headings and velocities
            if(Math.random() < chance_of_heading_change)
                heading += -0.5+Math.random();
            if(Math.random() < chance_of_velocity_change)
                velocity += Math.sin(Math.random());
            // update positions
            y += velocity * Math.sin(heading);
            x += velocity * Math.cos(heading);
            // reflect off document edges
            if (x < world_boundary.left || x > world_boundary.right ||
                y < world_boundary.top || y > world_boundary.bottom) {
                if (x < world_boundary.left) x = world_boundary.left;
                if (x > world_boundary.right) x = world_boundary.right;
                if (y < world_boundary.top) y = world_boundary.top;
                if (y > world_boundary.bottom) y = world_boundary.bottom;
                heading += Math.PI;
            }
            heading = heading % (2*Math.PI);
            $(this).reposition(x, y, heading+base_guitar_rotation);
            velocity -= velocity*damping_factor;
            if (Math.abs(velocity) < velocity_epsilon)
                velocity = 0.5;
            $(this).attr({data_heading: heading, data_velocity: velocity});
        });
    }

    var subject_states = ['bored', 'accepting', 'shredding'],
        object_states = ['drifting', 'dragged', 'held'];
    $.fn.pd_state = function(state) {
        var states = this.hasClass('subject') ? subject_states : object_states;
        this.removeClass($.grep(states, function(v){return v!=state;}).join(' '));
        this.addClass(state);
        return this;
    };

    $.fn.reposition = function(x, y) {
        if (arguments.length == 1) { y = x.top; x = x.left; }
        this.attr({data_x: x, data_y:y});
        this.css({top:y.toString()+'px', left:x.toString()+'px'});
        if (arguments.length == 3 && rotation_style)
            this.css(rotation_style, 'rotate('+arguments[2].toString()+'rad)');
        return this;
    };

    function cancel_all_drags() {
        $('.pd.object.dragged').each(function(i) {
            alert($(this));     // XXX remove before release
            $(this).pd_state('drifting');
        });
    }

    function get_points(s) {
        return $.map(s.split(','), function(e){return parseInt(e,10)});
    }

    function start_drag(object, drag) {
        // XXX: should default to [(object.width()/2),(object.height()/2)];
        var handle = get_points(object.attr('data_handle'));
        var o = {
            drag: drag,
            origin: {top:(drag.top-handle[1]), left:(drag.left-handle[0])},
            object: object,
            subject: null
        };
        cancel_all_drags();
        o.object.reposition(o.origin);
        o.object.pd_state('dragged');
        $(document).bind('mousemove mouseout', function(e){
            var top = o.origin.top+(e.pageY-o.drag.top),
                left = o.origin.left+(e.pageX-o.drag.left);
            o.object.reposition(left, top);
            // XXX check intersection with all subjects
            $('.pd.subject').each(function(i){
                if($(this).hasClass('shredding')) return;
                if(intersect_p($(this), o.object)) {
                    o.subject = $(this).pd_state('accepting');
                } else {
                    $(this).pd_state('bored');
                    o.subject = null;
                }
            });
            // set states appropriately
            return false;
        });
        $(document).one('mouseup', function(e) {
            $(this).unbind('mousemove mouseout');
            if(o.subject) {
                var snap = get_points(o.subject.attr('data_snap_pt')),
                    handle = get_points(o.object.attr('data_handle')),
                    top = o.subject.offset().top + snap[1] - handle[1],
                    left = o.subject.offset().left + snap[0] - handle[0];
                o.object.reposition(left, top, 0).pd_state('held');
                o.subject.pd_state('shredding');
                if (o.object.attr('data_theme'))
                    set_theme(o.object.attr('data_theme'));
            } else {
                // XXX check intersection with all subjects to avoid
                // dropping too close to a shredding subject; should
                // be able to only do this once, in mousemove
                $('.pd.subject').each(function(i){
                    if(intersect_p($(this), o.object)) {
                        // XXX better would be to drop somewhere near the feet of the subject, perhaps scatter in an arc around the feet
                        o.object.reposition(($(this).offset().left-o.object.width()),
                                            ($(this).offset().top+$(this).height()/2+($(this).height()/2)*Math.random()));
                    }
                });
                o.object.pd_state('drifting');
            }
            // if we're over an accepting subject, drop the guitar there
            return false;
        });
        return false;
    }

    function set_theme(theme) {
	$('#theme').attr('href', '/blog/'+theme+'.css');
    }

    $(document).ready(function () {
        detect_rotation_support();
        world_boundary.bottom = $(document).height();
        world_boundary.right = $(document).width();
        window.setInterval(animation_loop, animation_interval);

        $('.pd.object.scatter').each(function(i){
            var y = (world_boundary.bottom-$(this).height())*Math.random(),
                x = (world_boundary.right-$(this).width())*Math.random(),
                theta = 2*Math.PI*Math.random();
            $(this).reposition(x/2, y/2, theta);
            $(this).attr({data_heading: theta, data_velocity:15*Math.random()});
            $(this).removeClass('scatter');
            $(this).addClass('drifting');
        });

        $('.pd.object.drifting').live('mousedown', function(e) {
            return start_drag($(this), { top: e.pageY, left:e.pageX });
        });
        $('.pd.subject.shredding').live('mousedown', function(e){
            $(this).pd_state('accepting');
            set_theme('princess');
            // XXX hack -- need to support multiple subjects at some point.
            return start_drag($('.pd.object.held'), { top: e.pageY, left:e.pageX });
        });
    });
})(jQuery);
