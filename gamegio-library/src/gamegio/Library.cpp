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
#include "JoystickCbor.hpp"
#include "Joystick.hpp"
#include "SceneItemCbor.hpp"
#include "SceneItem.hpp"
#include "SkyboxItemCbor.hpp"
#include "SkyboxItem.hpp"
#include "SpriteItemCbor.hpp"
#include "Text3DItem.hpp"
#include "Text3DItemCbor.hpp"
#include "LogMessageItemCbor.hpp"
#include "LogMessageItem.hpp"
#include "ParticlesItemCbor.hpp"
#include "ParticlesItem.hpp"
#include "ExitRequestedItem.hpp"
#include "WindowEventCbor.hpp"
#include "MouseItem.hpp"
#include "KeysItem.hpp"
#include "ScreenModeItem.hpp"
#include "HoverItem.hpp"
#include "HoverCbor.hpp"
#include "DragItem.hpp"

#include "GUIElements.hpp"
#include "ButtonCbor.hpp"
#include "CheckBoxCbor.hpp"
#include "DropDownListCbor.hpp"
#include "EditTextCbor.hpp"
#include "StaticTextCbor.hpp"
#include "SliderCbor.hpp"
#include "UIElementCbor.hpp"


Library::Library()
{
    // enter all factories here
    _factories[ctGraphics3DConfig] = new Graphics3DSystemFactory();
    _factories[ctVolume] = new VolumeItemFactory();
    _factories[ctSoundSource] = new SoundSourceItemFactory();
    _factories[ctSoundListener] = new SoundListenerItemFactory();
    _factories[ctInputEventHandler] = new IEHClassFactory();
    _factories[ctCamera] = new CameraItemFactory();
    _factories[ctLight] = new LightItemFactory();
    _factories[ctGeometry] = new GeometryItemFactory();
    _factories[ctButton] = new ButtonItemFactory();
    _factories[ctCheckBox] = new CheckBoxItemFactory();
    _factories[ctDropDownList] = new DropDownListItemFactory();
    _factories[ctEditText] = new EditTextItemFactory();
    _factories[ctSlider] = new SliderItemFactory();
    _factories[ctStaticText] = new TextItemFactory();
    _factories[ctUIElement] = new HasUIElementFactory();
    _factories[ctMouseConfig] = new MouseFactory();
    _factories[ctGraphicsElement] = new HasNodeFactory();
    _factories[ctJoystick] = new JoystickFactory();
    _factories[ctScene] = new SceneItemFactory();
    _factories[ctSkybox] = new SkyboxItemFactory();
    _factories[ctSprite] = new SpriteItemFactory();
    _factories[ctText3D] = new Text3DItemFactory();
    _factories[ctLogMessage] = new LogMessageItemFactory();
    _factories[ctParticles] = new ParticlesItemFactory();
    _factories[ctExitRequestedEvent] = new ExitRequestedItemFactory();
    _factories[ctMouse] = new MouseItemFactory();
    _factories[ctKeyEvent] = new KeysItemFactory();
    _factories[ctScreenModeEvent] = new ScreenModeItemFactory();
    _factories[ctHoverEvent] = new HoverItemFactory();
    _factories[ctDragEvent] = new DragItemFactory();
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
  //              Implement     Imlement           Event
  //              Class         Class CT           Name (ctEvent, registerEventFunction)

    GIO_REG_EVENT(MouseItem, Mouse, MouseEvent)
    GIO_REG_EVENT(KeysItem, KeyEvent, KeyEvent)
    GIO_REG_EVENT(ScreenModeItem, ScreenModeEvent, ScreenModeEvent)
    GIO_REG_EVENT(EditTextItem, EditText, EditText)
    GIO_REG_EVENT(CheckBoxItem, CheckBox, CheckBox)
    GIO_REG_EVENT(ButtonItem, Button, Button)
    GIO_REG_EVENT(SliderItem, Slider, Slider)
    GIO_REG_EVENT(DropDownListItem, DropDownList, DropDownList)
    GIO_REG_EVENT(Joystick, Joystick, JoystickEvent)
    GIO_REG_EVENT(ExitRequestedItem, ExitRequestedEvent, ExitRequestedEvent)
    GIO_REG_EVENT(HoverItem, HoverEvent, HoverEvent)
    GIO_REG_EVENT(DragItem, DragEvent, DragEvent)

    // old input event handler, deprecated, compatibility mode
    GIO_REG_EVENT(IEHClass, InputEventHandler, MouseEvent090)
    GIO_REG_EVENT(IEHClass, InputEventHandler, KeyEvent090)
    GIO_REG_EVENT(IEHClass, InputEventHandler, ExitRequestedEvent090)
}

