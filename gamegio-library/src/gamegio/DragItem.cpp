// Mouse Drag entities in 3D for HGamer3D
//
// This file has been released by its author to the public domain
// For details, see https://creativecommons.org/publicdomain/zero/1.0/ 
//
// file: HGamer3D/gamegio-library/src/gamegio/DragItem.cpp

#include <iostream>

#include "DragItem.hpp"

#include "Graphics3DSystem.hpp"
#include "Vec3Cbor.hpp"
#include "EntityIdCbor.hpp"

using namespace std;
using namespace cbd;

GIO_METHOD_FUNC(DragItem, DragHitPosition) //input
GIO_METHOD_FUNC(DragItem, DragCamera) //input
GIO_METHOD_FUNC(DragItem, DragEvent) //output

GCO_FACTORY_IMP(DragItem)
  GCO_FACTORY_METHOD(DragItem, ctDragCamera, DragCamera)
  GCO_FACTORY_METHOD(DragItem, ctDragHitPosition, DragHitPosition)
  GCO_FACTORY_METHOD(DragItem, ctDragEvent, DragEvent)
GCO_FACTORY_IMP_END

const uint64_t ctDragCamera = 0x87c2f05611cb2d50;
const uint64_t ctDragHitPosition = 0x3fd1f759fcea70b4;
const uint64_t ctDragEvent = 0xa855430c4f4b1212;

// DragItem is a transitory entity that only exists during a drag;
// the drag starts when it is created and ends when it's deleted.
// Caller determines when to start/end drag.
// This component provides a 3D vector in world coordinates
// of relative movement from the provided hit position
// to a line from the mouse into the scene. Caller is responsible
// for moving the object during and/or after the drag.
// It is intended the caller can also drag multiple objects at once
// using the same vector

DragItem::DragItem() : Object(Graphics3DSystem::getG3DS()->context), hitPosition(Vector3(0.0f, 0.0f, 0.0f)), camera(NULL), dragEventF(NULL)
{
}

DragItem::~DragItem()
{
    UnsubscribeFromAllEvents();
}

FrItem DragItem::msgCreate(FrMsg m, FrMsgLength l)
{
    return new DragItem();
}

void DragItem::msgDestroy()
{
    delete this;
}

void DragItem::msgDragHitPosition(FrMsg m, FrMsgLength l)
{
  Position pos;
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  readPosition(&it, &pos);

  hitPosition.x_ = pos.x;
  hitPosition.y_ = pos.y;
  hitPosition.z_ = pos.z;

  cout << "DragItem::msgDragHitPosition- received position (" << hitPosition.x_ << "," << hitPosition.y_ << "," << hitPosition.z_;
}

void DragItem::msgDragCamera(FrMsg m, FrMsgLength l)
{
  if (camera != NULL)
    return;
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  EntityId cameraId;
  readEntityId(&it, &cameraId);

  Graphics3DSystem *g3ds = Graphics3DSystem::getG3DS();
  map<EntityId, Node*>::iterator newCamera = g3ds->node_map.find(cameraId);

  if (newCamera != g3ds->node_map.end())
  {
    camera = newCamera->second->GetComponent<Camera>();

    SubscribeToEvent(E_MOUSEMOVE, URHO3D_HANDLER(DragItem, HandleMouseMove));
  }
}

void DragItem::HandleMouseMove(StringHash eventType, VariantMap& eventData)
{
  if (NULL == camera)
    return;

  // (unlike Hover) okay if mouse invisible, caller might want to better show object being dragged

  int mx = eventData[MouseMove::P_X].GetInt();
  int my = eventData[MouseMove::P_Y].GetInt();

  Graphics3DSystem *g3ds = Graphics3DSystem::getG3DS();
  Context *context = g3ds->context;
  Graphics* graphics = context->GetSubsystem<Graphics>();
  Ray cameraRay = camera->GetScreenRay((float)mx / graphics->GetWidth(),
                                       (float)my / graphics->GetHeight());

  // where would this point go to line up under the mouse
  Vector3 newPoint = cameraRay.Project(hitPosition);

  // convert to structure HGamer3D can read. Using C99
  Position newPosition = 
    { .x = newPoint.x_, 
      .y = newPoint.y_,
      .z = newPoint.z_ };

  // send the coords to Fresco & notify event listeners
  if (dragEventF != NULL)
  {
    uint8_t buf[64];
    CborEncoder encoder;
    cbor_encoder_init(&encoder, buf, sizeof(buf), 0);
    cbd::writePosition(&encoder, newPosition); // in Vec3Cbor
    size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
    dragEventF(dragDataP, dragEventType, buf, len);
  }
}

void DragItem::msgDragEvent(FrMsg m, FrMsgLength l)
{
  // component is for sending events only, not receiving them
}

void DragItem::registerDragEventFunction(FrMessageFn2 f, void* p2, uint64_t dragET)
{
    dragEventF = f;
    dragDataP = p2;
    dragEventType = dragET;
    cout << "registered drag event function " << f << "\n";
}
