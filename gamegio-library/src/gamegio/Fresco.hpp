//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/Fresco.hpp

#ifndef __fresco_hpp
#define __fresco_hpp

#include <stdint.h>
#include <map>

extern "C" {

//
// generell Fresco types
//

// a CBor Message, bytes and length

typedef uint8_t* FrMsg;
typedef uint32_t FrMsgLength;

/*
    Components will be used later and they can be either an Item
    or an Item Property. They have a type, which is denoted by
    an uint64_t.
*/

typedef uint64_t FrComponentType;
typedef FrComponentType FrItemType;
typedef FrComponentType FrPropertyType;
typedef FrComponentType FrEventType;

typedef void* FrItem;
typedef void* FrEntity;
typedef void (*FrMessageFn) (FrItem it, FrMsg m, FrMsgLength l);
typedef void (*FrMessageFn2) (FrEntity e, FrEventType pr, FrMsg m, FrMsgLength l);

/*
    giornata (object lib libraries) interface

    A C++ object library exhibits this interface, to enable working 
    with the exposed items.

    There are some simple functions to create and destroy items. There
    is a function to get the function for setting properties of an item.
    And there is a function to set a property change callback. 
*/

#ifdef _WIN32
#define FRESCO_EXPORT __declspec(dllexport)
#else
#define FRESCO_EXPORT
#endif

FRESCO_EXPORT FrItem gioCreateItem(FrItemType ct, FrMsg m, FrMsgLength l);
FRESCO_EXPORT void gioDestroyItem(FrComponentType ct, FrItem it);
FRESCO_EXPORT FrMessageFn gioGetMsgSender(FrComponentType ob, FrComponentType pr);
FRESCO_EXPORT void gioRegisterMsgReceiver(FrItemType ctItem, FrEventType ctEvent, FrItem item, FrEntity receiver, FrMessageFn2 f);

/*
FrItem gioCreateItem(FrItemType ct, FrMsg m, FrMsgLength l);
void gioDestroyItem(FrItemType ct, FrItem it);
FrMessageFn gioGetMsgSender(FrItemType ob, FrPropertyType pr); 
void gioRegisterMsgReceiver(FrItemType ct, FrEventType pr, FrItem it, FrEntity rcv, FrMessageFn2);
*/

typedef std::map<FrPropertyType, FrMessageFn> PropertyMap;
typedef std::map<FrItemType, PropertyMap> ComponentPropertyMap;

typedef FrItem* (*ObjectCreateFn) (FrMsg m, FrMsgLength l);
typedef std::map<FrComponentType, ObjectCreateFn> CreatorMap;

}  // extern "C"

class GCOFactory {
    public:
        GCOFactory();
        ~GCOFactory();
        virtual FrItem createItem(FrMsg m, FrMsgLength l);
        virtual void destroyItem(FrItem item);
        virtual FrMessageFn getMessageFn(FrComponentType pt);
};

#define GCO_FACTORY_DEC(CNAME) \
class CNAME ## Factory : public GCOFactory {\
    public:\
        virtual FrItem createItem(FrMsg m, FrMsgLength l);\
        virtual void destroyItem(FrItem item);\
        virtual FrMessageFn getMessageFn(FrComponentType pt);\
};

#define GCO_FACTORY_IMP(CNAME) \
FrItem CNAME ## Factory::createItem(FrMsg m, FrMsgLength l)\
{\
    return CNAME ::msgCreate(m, l);\
}\
void CNAME ## Factory::destroyItem(FrItem item)\
{\
    ((CNAME *)item)->msgDestroy ();\
}\
FrMessageFn CNAME ## Factory::getMessageFn(FrComponentType pt)\
{

#define GCO_FACTORY_IMP_END return 0; }

#define GCO_FACTORY_METHOD(CNAME, CT, MNAME) \
    if (pt == CT) { return GIO_METHOD_FUNCN(CNAME, MNAME); }

#define GIO_METHOD_FUNCN(CNAME, MNAME) CNAME ## _ ## msg ## MNAME

#define GIO_METHOD_DEC(CNAME, MNAME) \
extern "C" void GIO_METHOD_FUNCN(CNAME, MNAME)(void* item, FrMsg m, FrMsgLength l);

#define GIO_METHOD_FUNC(CNAME, MNAME) \
void GIO_METHOD_FUNCN(CNAME, MNAME)(void* item, FrMsg m, FrMsgLength l) {\
((CNAME*)item)->msg ## MNAME(m,l); };

#define GIO_REG_EVENT(CNAME, TNAME, EVENT) \
  if (ctItem == ct ## TNAME && ctEvent == ct ## EVENT) {\
    ((CNAME*)item)->register ## EVENT ## Function(f, receiver, ct ## EVENT);\
    return;\
  }


#endif
