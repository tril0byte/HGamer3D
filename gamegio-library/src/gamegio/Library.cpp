//	C++ part of bindings for graphics
//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2017 Peter Althainz
//	
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at 
//	http://www.apache.org/licenses/LICENSE-2.0)
// 
//	file: gamegio-library/src/Library.cpp

#include "Library.hpp"
#include "Graphics3DSystem.hpp"
#include "Graphics3DConfigCbor.hpp"
#include "VolumeItem.hpp"
#include "SoundSourceItem.hpp"
#include "SoundSourceCbor.hpp"
#include "SoundListenerItem.hpp"
#include "InputEventHandler.hpp"
#include "InputEventHandlerCbor.hpp"
#include "KeyEventCbor.hpp"
#include "MouseCbor.hpp"
#include "SoundListenerCbor.hpp"
#include "VolumeCbor.hpp"
#include "CameraCbor.hpp"
#include "LightCbor.hpp"
#include "GeometryCbor.hpp"
#include "CameraItem.hpp"
#include "LightItem.hpp"
#include "GeometryItem.hpp"
#include "DropDownListItem.hpp"

#include "GUIElements.hpp"
#include "ButtonCbor.hpp"
#include "CheckBoxCbor.hpp"
#include "DropDownListCbor.hpp"
#include "EditTextCbor.hpp"
#include "StaticTextCbor.hpp"
#include "SliderCbor.hpp"
#include "UIElementCbor.hpp"
#include "UIEventCbor.hpp"


Library::Library()
{
    // enter all factories here
    _factories[ctGraphics3DConfig] = new Graphics3DSystemFactory();
    _factories[ctVolume] = new VolumeItemFactory();
    _factories[ctSoundSource] = new SoundSourceItemFactory();
    _factories[ctSoundListener] = new SoundListenerItemFactory();
    _factories[ctInputEventHandler] = new IEHClassFactory();
    _factories[ctInputEvents] = new BasicEventHandlerFactory();
    _factories[ctCamera] = new CameraItemFactory();
    _factories[ctLight] = new LightItemFactory();
    _factories[ctGeometry] = new GeometryItemFactory();
    _factories[ctButton] = new ButtonItemFactory();
    _factories[ctBasicButton] = new StandardButtonItemFactory();
    _factories[ctImageButton] = new ImageButtonItemFactory();
    _factories[ctCheckBox] = new CheckBoxItemFactory();
    _factories[ctDropDownList] = new DropDownListItemFactory();
    _factories[ctEditText] = new EditTextItemFactory();
    _factories[ctSlider] = new SliderItemFactory();
    _factories[ctStaticText] = new TextItemFactory();
    _factories[ctUIElement] = new HasUIElementFactory();
    _factories[ctMouseConfig] = new MouseFactory();
    _factories[ctGraphicsElement] = new HasNodeFactory();
    _factories[ctWindowGUI] = new WindowGUIFactory();
    _factories[ctTooltip] = new TooltipFactory();
}

Library::~Library()
{

}

GCOFactory* Library::getFactory(FrComponentType ct)
{
    std::map<FrItemType, GCOFactory*>::iterator iter = _factories.find(ct);
    if (iter != _factories.end())
    {
        return iter->second;
    }
    return NULL;
}

Library theLibrary = Library();


FrItem gioCreateItem(FrItemType ct, FrMsg m, FrMsgLength l)
{
    GCOFactory* f = theLibrary.getFactory(ct);
    if (f != NULL) {
        return f->createItem(m, l);
    }
    else {
        return NULL;
    }
}

void gioDestroyItem(FrComponentType ct, FrItem it)
{
    theLibrary.getFactory(ct)->destroyItem(it);
}

FrMessageFn gioGetMsgSender(FrComponentType ob, FrComponentType pr)
{
    GCOFactory* f = theLibrary.getFactory(ob);
    if (f != NULL) {
        return f->getMessageFn(pr);
    }
    else {
        return NULL;
    }
}

void gioRegisterMsgReceiver(FrItemType ctItem, FrEventType ctEvent, FrItem item, FrEntity receiver, FrMessageFn2 f)
{
    // old api, for compatibility
    GIO_REG_EVENT(IEHClass, InputEventHandler, MouseEvent)
    GIO_REG_EVENT(IEHClass, InputEventHandler, KeyEvent)
    GIO_REG_EVENT(IEHClass, InputEventHandler, ExitRequestedEvent)
    // new event api, based on InputEvents ct
    GIO_REG_EVENT(BasicEventHandler, InputEvents, MouseMoveEvent)
    GIO_REG_EVENT(BasicEventHandler, InputEvents, MouseClickEvent)
    GIO_REG_EVENT(BasicEventHandler, InputEvents, MouseWheelEvent)
    GIO_REG_EVENT(BasicEventHandler, InputEvents, KeyEvent)
    GIO_REG_EVENT(BasicEventHandler, InputEvents, ExitRequestedEvent)
    GIO_REG_EVENT(BasicEventHandler, InputEvents, UIClickEvent)

    GIO_REG_EVENT(EditTextItem, EditText, EditText)
    GIO_REG_EVENT(CheckBoxItem, CheckBox, CheckBox)

    GIO_REG_EVENT(ButtonItem, Button, Button)
    GIO_REG_EVENT(StandardButtonItem, BasicButton, ButtonEvent)
    GIO_REG_EVENT(StandardButtonItem, ImageButton, ButtonEvent)

    GIO_REG_EVENT(SliderItem, Slider, Slider)
    GIO_REG_EVENT(DropDownListItem, DropDownList, DropDownList)

    // hover & drag for all elements
    GIO_REG_EVENT(HasUIElement, UIElement, UIHoverEvent)
    GIO_REG_EVENT(HasUIElement, UIElement, UIDragEvent)
    
    GIO_REG_EVENT(HasUIElement, StaticText, UIHoverEvent)
    GIO_REG_EVENT(HasUIElement, StaticText, UIDragEvent)
    
    GIO_REG_EVENT(HasUIElement, EditText, UIHoverEvent)
    GIO_REG_EVENT(HasUIElement, EditText, UIDragEvent)

    GIO_REG_EVENT(HasUIElement, Button, UIHoverEvent)
    GIO_REG_EVENT(HasUIElement, Button, UIDragEvent)
    
    GIO_REG_EVENT(HasUIElement, BasicButton, UIHoverEvent)
    GIO_REG_EVENT(HasUIElement, BasicButton, UIDragEvent)

    GIO_REG_EVENT(HasUIElement, ImageButton, UIHoverEvent)
    GIO_REG_EVENT(HasUIElement, ImageButton, UIDragEvent)

    GIO_REG_EVENT(HasUIElement, Slider, UIHoverEvent)
    GIO_REG_EVENT(HasUIElement, Slider, UIDragEvent)
    
    GIO_REG_EVENT(HasUIElement, CheckBox, UIHoverEvent)
    GIO_REG_EVENT(HasUIElement, CheckBox, UIDragEvent)
    
    GIO_REG_EVENT(HasUIElement, WindowGUI, UIHoverEvent)
    GIO_REG_EVENT(HasUIElement, WindowGUI, UIDragEvent)
    
    GIO_REG_EVENT(HasUIElement, DropDownList, UIHoverEvent)
    GIO_REG_EVENT(HasUIElement, DropDownList, UIDragEvent)
}

