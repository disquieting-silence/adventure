---
title: Engine for Adventure
---

* [Language](#langauge)
* [Turns](#turns)
* [Actions](#actions)
* [Transformer Stack](#stack)
* [Movement](#movement)
* [Items](#items)
* [Changing the World](#changing-the-world)

### Language

(Insert Content)

### Turns

(Insert Content)

### Actions

(Insert Content)
(synonym logic)

### Stack

(Insert Content)
(Overly complex - just really for learning rather than purpose)

### Movement

#### Transitions
The player can move in between the rooms when there is a `Transition` defined for moving from the current room to another room. Note, `Transition` possibilities can change during the game due to keys and bad choices.

In an attempt to improve the efficiency a small amount, a `Map` is used for storing the transitions. The `Map` maps from `(Room, Direction)` pairs to `Room`. The reason it is a map is because it allows faster read access than a cons list.

The process is fairly simple. The direction that the user has requested is paired with the current room to see if there are any `Transition` mappings for that pair. If there are not, the player cannot go in that direction. If there is, then the player moves to the room which is the `value` of that key.

Note, that the `Transition` lookup table is also used for identifying which exits are present when &apos;describing&apos; the room.

### Items

This was probably not a good idea. Much like a database, there are two collections for item information: the item data type, and the `ItemInfo`. Each item data constructor is a key into the `ItemCollection` map that yields the `ItemInfo` for that item.

The information stored about an item in `ItemInfo` is as follows:

* `getItem`: item (`Item`)
* `getName`: item name (`ItemName String`)
* `getRoom`: potential room (`Maybe Room`)
* `getDesc`: item description (`ItemDesc String`)

Although at this stage, the only thing that has to change is the room, the engine is designed to be able to easily change the name or description of an item. Imagine there might be a box which was closed and is now open. We want to be able to make that change in representation effortlessly. For more information, see [Changing the World](#changing-the-world).

### Changing the World

(Insert Content)
