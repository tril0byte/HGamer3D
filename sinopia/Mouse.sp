enum MouseMode {
    Absolute;
    Relative;
    Wrap;
}

struct MouseConfig {
    mode : MouseMode;
}

id64 MouseConfig = 0xa532f43b1c1c6bc7

type Mouse = MouseMode
id64 Mouse = 0x07669c3c292bf265

struct MouseButtonData {
    button : Int32;
    buttons : Int32;
    qualifiers : Int32;
}

struct MouseMoveData {
    x : Int32;  # only when mouse visible
    y : Int32; 
    dx : Int32;
    dy : Int32;
    buttons : Int32;
    qualifiers : Int32;
}

struct MouseWheelData {
    wheel : Int32;
    buttons : Int32;
    qualifiers : Int32;
}

enum MouseEvent {
    NoMouseEvent;
    MouseButtonUpEvent MouseButtonData;
    MouseButtonDownEvent MouseButtonData;
    MouseMoveEvent MouseMoveData;
    MouseWheelEvent MouseWheelData;
}

id64 MouseEvent090 = 0x27eaf3fd46595d08
id64 MouseEvent = 0x8a73da7bdfbe4ccc