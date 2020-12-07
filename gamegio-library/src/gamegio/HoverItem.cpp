// Mouse Hover over entities in 3D for HGamer3D
//
// This file has been released by its author to the public domain
// For details, see https://creativecommons.org/publicdomain/zero/1.0/ 
//
// file: HGamer3D/gamegio-library/src/gamegio/Hover.cpp

#include <iostream>

#include "HoverItem.hpp"

#include "Graphics3DSystem.hpp"
#include "EntityIdCbor.hpp"

using namespace std;
using namespace cbd;

// defines CNAME_msgMNAME C wrapper around C++ method.
// Called when a value is assigned to the component
GIO_METHOD_FUNC(HoverItem, HoverCamera)
GIO_METHOD_FUNC(HoverItem, HoverEvent)

// defines CNAMEFactory class
GCO_FACTORY_IMP(HoverItem)
// performs component type checking, then invokes the method
  GCO_FACTORY_METHOD(HoverItem, ctHoverCamera, HoverCamera)
  GCO_FACTORY_METHOD(HoverItem, ctHoverEvent, HoverEvent)
GCO_FACTORY_IMP_END

HoverItem::HoverItem() : Object(Graphics3DSystem::getG3DS()->context), camera(NULL), hoverEventF(NULL)
{
}

HoverItem::~HoverItem()
{
    UnsubscribeFromAllEvents();
}

FrItem HoverItem::msgCreate(FrMsg m, FrMsgLength l)
{
    return new HoverItem();
}

void HoverItem::msgDestroy()
{
    delete this;
}

void HoverItem::msgHoverCamera(FrMsg m, FrMsgLength l)
{
  cout << "HoverItem::msgHoverCamera invoked, this=" << this << "\n";
  if (camera != NULL)
    return;
  CborParser parser; CborValue it;
  cbor_parser_init(m, l, 0, &parser, &it);
  EntityId cameraId;
  readEntityId(&it, &cameraId);
  cout << "HoverItem::msgHoverCamera - entity id length = " << cameraId.size() << "\n";

  Graphics3DSystem *g3ds = Graphics3DSystem::getG3DS();
  map<EntityId, Node*>::iterator newCamera = g3ds->node_map.find(cameraId);

  if (newCamera != g3ds->node_map.end())
  {
    //std::cout << "HoverItem_msgSetCamera: New Camera found!\n";
    camera = newCamera->second->GetComponent<Camera>();

    SubscribeToEvent(E_MOUSEMOVE, URHO3D_HANDLER(HoverItem, HandleMouseMove));
  }
  else
    {
      //    std::cout << "HoverItem_msgSetCamera: camera id not found\n";
    }
}

void HoverItem::HandleMouseMove(StringHash eventType, VariantMap& eventData)
{
    const float maxDistance = 250.0f;

    if (NULL == camera)
        return;

    Graphics3DSystem *g3ds = Graphics3DSystem::getG3DS();
    Context *context = g3ds->context;
    Input *input = context->GetSubsystem<Input>();
    if (!input->IsMouseVisible()) // Mouse can't be over anything if it's not shown
        // consider setting the hover entity to None here?
        return;

    int mx = eventData[MouseMove::P_X].GetInt();
    int my = eventData[MouseMove::P_Y].GetInt();

    UI* ui = context->GetSubsystem<UI>();
    if (ui->GetElementAt(mx, my, true)) // not interested in UI elements under cursor
        // probably don't want to set the hover entity to None here and trigger an event
	// in case the same entity is still under the cursor on the other side of the UI Element
	return;
    Graphics* graphics = context->GetSubsystem<Graphics>();
    Ray cameraRay = camera->GetScreenRay((float)mx / graphics->GetWidth(),
					 (float)my / graphics->GetHeight());
    // Pick only geometry objects, not zones or lights, only the closest hit
    PODVector<RayQueryResult> results;
    RayOctreeQuery query(results, cameraRay, RAY_TRIANGLE, maxDistance, DRAWABLE_GEOMETRY);
    Scene* scene = g3ds->scene;
    scene->GetComponent<Octree>()->RaycastSingle(query);
    if (!results.Size())
    {   //here we do want to set the hover entity to none
        //if it wasn't already and notify event listeners.
        if (hoverEntity.size())
	{
	  uint8_t buf[64];
	  CborEncoder encoder;
	  cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

	  hoverEntity.clear();
	  cbd::HoverEvent noEntityEvent;
          noEntityEvent.selector = NoEntity;
	  cbd::writeHoverEvent(&encoder, noEntityEvent);
	  size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
	  cout << "HoverItem::HandleMouseMove about to callback, Over No Entity\n";
	  hoverEventF(hoverDataP, hoverEventType, buf, len);
	}
        return;
    }
    RayQueryResult& result = results[0];
    Node* hitNode = result.node_;
    if (NULL == hitNode)
      return;
    // Read back the EntityId that was saved in HasNode.cpp - HasNode::msgEntityId()
    // just after the node was created
    PODVector<unsigned char> buf = hitNode->GetVar(StringHash(HOVER_ENTITY_HASHKEY)).GetBuffer();
    // Read stored HGamer3D EntityId out of the Urho3D Node 
    // so we can see what upper layer entity the Ray has hit 
    cbd::EntityId eid = BufferToEntityId(buf); 
    if (eid != hoverEntity)
    {
      // Over new entity, update stored EID
      hoverEntity = eid;
      cout << "HoverItem::HandleMouseMove over a new entity: ";
      printEID(eid);
    }
    // Trigger hover entity changed event - now done even when over same entity, to update hit position.
    if (hoverEventF != NULL)
    {
	uint8_t buf[64];
	CborEncoder encoder;
	cbor_encoder_init(&encoder, buf, sizeof(buf), 0);

	cbd::HoverEvent hev;
        hev.selector = JustEntity;
        hev.data.HoverEntity.value0 = eid;
        hev.data.HoverEntity.value1.x = result.position_.x_;
        hev.data.HoverEntity.value1.y = result.position_.y_;
        hev.data.HoverEntity.value1.z = result.position_.z_;
	cbd::writeHoverEvent(&encoder, hev);

	size_t len = cbor_encoder_get_buffer_size(&encoder, buf);
	cout << "HoverItem::HandleMouseMove about to callback, eid: ";
        printEID(eid);
	hoverEventF(hoverDataP, hoverEventType, buf, len);
	cout << "HoverItem::HandleMouseMove returned from callback and buffer len was " << len << "\n";
	cout << "HoverItem::HandleMouseMove - entity id length = " << eid.size() << "\n";
    }
}

bool HoverItem::CompareEntities(cbd::EntityId *a, cbd::EntityId *b)
{
  if (NULL == a && NULL == b)
    return true;
  if (NULL == a || NULL == b)
    return false;

  bool result = (*a == *b);
  cout << "CompareEntities: " << result << "\n";
  return result;
  /*
  EntityId::iterator ai, bi;
  ai = a->begin();
  bi = b->begin();
  while (ai != a->end())
  {
    
  }
  */
}

// accepts the new value when someone assigns a value to a ctHoverEvent property component.
void HoverItem::msgHoverEvent(FrMsg m, FrMsgLength l)
{
  // ignore messages, this component is for sending events, not receiving them
  cout << "HoverItem::msgHoverEvent invoked, this=" << this << "\n";
  
}

void HoverItem::registerHoverEventFunction(FrMessageFn2 f, void* p2, uint64_t hoverET)
{
    hoverEventF = f;
    hoverDataP = p2;
    hoverEventType = hoverET;
    cout << "registered hover event function " << f << "\n";
}
