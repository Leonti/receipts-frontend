
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

var _elm_lang$virtual_dom$Native_Debug = function() {


// IMPORT / EXPORT

function unsafeCoerce(value)
{
	return value;
}

var upload = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	var element = document.createElement('input');
	element.setAttribute('type', 'file');
	element.setAttribute('accept', 'text/json');
	element.style.display = 'none';
	element.addEventListener('change', function(event)
	{
		var fileReader = new FileReader();
		fileReader.onload = function(e)
		{
			callback(_elm_lang$core$Native_Scheduler.succeed(e.target.result));
		};
		fileReader.readAsText(event.target.files[0]);
		document.body.removeChild(element);
	});
	document.body.appendChild(element);
	element.click();
});

function download(historyLength, json)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var fileName = 'history-' + historyLength + '.txt';
		var jsonString = JSON.stringify(json);
		var mime = 'text/plain;charset=utf-8';
		var done = _elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0);

		// for IE10+
		if (navigator.msSaveBlob)
		{
			navigator.msSaveBlob(new Blob([jsonString], {type: mime}), fileName);
			return callback(done);
		}

		// for HTML5
		var element = document.createElement('a');
		element.setAttribute('href', 'data:' + mime + ',' + encodeURIComponent(jsonString));
		element.setAttribute('download', fileName);
		element.style.display = 'none';
		document.body.appendChild(element);
		element.click();
		document.body.removeChild(element);
		callback(done);
	});
}


// POPOUT

function messageToString(value)
{
	switch (typeof value)
	{
		case 'boolean':
			return value ? 'True' : 'False';
		case 'number':
			return value + '';
		case 'string':
			return '"' + addSlashes(value, false) + '"';
	}
	if (value instanceof String)
	{
		return '\'' + addSlashes(value, true) + '\'';
	}
	if (typeof value !== 'object' || value === null || !('ctor' in value))
	{
		return '…';
	}

	var ctorStarter = value.ctor.substring(0, 5);
	if (ctorStarter === '_Tupl' || ctorStarter === '_Task')
	{
		return '…'
	}
	if (['_Array', '<decoder>', '_Process', '::', '[]', 'Set_elm_builtin', 'RBNode_elm_builtin', 'RBEmpty_elm_builtin'].indexOf(value.ctor) >= 0)
	{
		return '…';
	}

	var keys = Object.keys(value);
	switch (keys.length)
	{
		case 1:
			return value.ctor;
		case 2:
			return value.ctor + ' ' + messageToString(value._0);
		default:
			return value.ctor + ' … ' + messageToString(value[keys[keys.length - 1]]);
	}
}


function primitive(str)
{
	return { ctor: 'Primitive', _0: str };
}


function init(value)
{
	var type = typeof value;

	if (type === 'boolean')
	{
		return {
			ctor: 'Constructor',
			_0: _elm_lang$core$Maybe$Just(value ? 'True' : 'False'),
			_1: true,
			_2: _elm_lang$core$Native_List.Nil
		};
	}

	if (type === 'number')
	{
		return primitive(value + '');
	}

	if (type === 'string')
	{
		return { ctor: 'S', _0: '"' + addSlashes(value, false) + '"' };
	}

	if (value instanceof String)
	{
		return { ctor: 'S', _0: "'" + addSlashes(value, true) + "'" };
	}

	if (value instanceof Date)
	{
		return primitive('<' + value.toString() + '>');
	}

	if (value === null)
	{
		return primitive('XXX');
	}

	if (type === 'object' && 'ctor' in value)
	{
		var ctor = value.ctor;

		if (ctor === '::' || ctor === '[]')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'ListSeq'},
				_1: true,
				_2: A2(_elm_lang$core$List$map, init, value)
			};
		}

		if (ctor === 'Set_elm_builtin')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'SetSeq'},
				_1: true,
				_2: A3(_elm_lang$core$Set$foldr, initCons, _elm_lang$core$Native_List.Nil, value)
			};
		}

		if (ctor === 'RBNode_elm_builtin' || ctor == 'RBEmpty_elm_builtin')
		{
			return {
				ctor: 'Dictionary',
				_0: true,
				_1: A3(_elm_lang$core$Dict$foldr, initKeyValueCons, _elm_lang$core$Native_List.Nil, value)
			};
		}

		if (ctor === '_Array')
		{
			return {
				ctor: 'Sequence',
				_0: {ctor: 'ArraySeq'},
				_1: true,
				_2: A3(_elm_lang$core$Array$foldr, initCons, _elm_lang$core$Native_List.Nil, value)
			};
		}

		var ctorStarter = value.ctor.substring(0, 5);
		if (ctorStarter === '_Task')
		{
			return primitive('<task>');
		}

		if (ctor === '<decoder>')
		{
			return primitive(ctor);
		}

		if (ctor === '_Process')
		{
			return primitive('<process>');
		}

		var list = _elm_lang$core$Native_List.Nil;
		for (var i in value)
		{
			if (i === 'ctor') continue;
			list = _elm_lang$core$Native_List.Cons(init(value[i]), list);
		}
		return {
			ctor: 'Constructor',
			_0: ctorStarter === '_Tupl' ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(ctor),
			_1: true,
			_2: _elm_lang$core$List$reverse(list)
		};
	}

	if (type === 'object')
	{
		var dict = _elm_lang$core$Dict$empty;
		for (var i in value)
		{
			dict = A3(_elm_lang$core$Dict$insert, i, init(value[i]), dict);
		}
		return { ctor: 'Record', _0: true, _1: dict };
	}

	return primitive('XXX');
}

var initCons = F2(initConsHelp);

function initConsHelp(value, list)
{
	return _elm_lang$core$Native_List.Cons(init(value), list);
}

var initKeyValueCons = F3(initKeyValueConsHelp);

function initKeyValueConsHelp(key, value, list)
{
	return _elm_lang$core$Native_List.Cons(
		_elm_lang$core$Native_Utils.Tuple2(init(key), init(value)),
		list
	);
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	upload: upload,
	download: F2(download),
	unsafeCoerce: unsafeCoerce,
	messageToString: messageToString,
	init: init
}

}();

var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

var _elm_lang$virtual_dom$VirtualDom_Helpers$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom_Helpers$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom_Helpers$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom_Helpers$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom_Helpers$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom_Helpers$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom_Helpers$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom_Helpers$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom_Helpers$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom_Helpers$onClick = function (msg) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$virtual_dom$VirtualDom_Helpers$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom_Helpers$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom_Helpers$id = _elm_lang$virtual_dom$VirtualDom_Helpers$attribute('id');
var _elm_lang$virtual_dom$VirtualDom_Helpers$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom_Helpers$class = function (name) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$property,
		'className',
		_elm_lang$core$Json_Encode$string(name));
};
var _elm_lang$virtual_dom$VirtualDom_Helpers$href = function (name) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$property,
		'href',
		_elm_lang$core$Json_Encode$string(name));
};
var _elm_lang$virtual_dom$VirtualDom_Helpers$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom_Helpers$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom_Helpers$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom_Helpers$div = _elm_lang$virtual_dom$VirtualDom_Helpers$node('div');
var _elm_lang$virtual_dom$VirtualDom_Helpers$span = _elm_lang$virtual_dom$VirtualDom_Helpers$node('span');
var _elm_lang$virtual_dom$VirtualDom_Helpers$a = _elm_lang$virtual_dom$VirtualDom_Helpers$node('a');
var _elm_lang$virtual_dom$VirtualDom_Helpers$h1 = _elm_lang$virtual_dom$VirtualDom_Helpers$node('h1');
var _elm_lang$virtual_dom$VirtualDom_Helpers$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Helpers$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom_Helpers$Property = {ctor: 'Property'};

var _elm_lang$virtual_dom$VirtualDom_Expando$purple = _elm_lang$virtual_dom$VirtualDom_Helpers$style(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'color', _1: 'rgb(136, 19, 145)'},
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$blue = _elm_lang$virtual_dom$VirtualDom_Helpers$style(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'color', _1: 'rgb(28, 0, 207)'},
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$red = _elm_lang$virtual_dom$VirtualDom_Helpers$style(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'color', _1: 'rgb(196, 26, 22)'},
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$leftPad = function (maybeKey) {
	var _p0 = maybeKey;
	if (_p0.ctor === 'Nothing') {
		return _elm_lang$virtual_dom$VirtualDom_Helpers$style(
			{ctor: '[]'});
	} else {
		return _elm_lang$virtual_dom$VirtualDom_Helpers$style(
			{
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'padding-left', _1: '4ch'},
				_1: {ctor: '[]'}
			});
	}
};
var _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow = function (arrow) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$span,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$style(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'color', _1: '#777'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'padding-left', _1: '2ch'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'width', _1: '2ch'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'display', _1: 'inline-block'},
								_1: {ctor: '[]'}
							}
						}
					}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(arrow),
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Expando$lineStarter = F3(
	function (maybeKey, maybeIsClosed, description) {
		var arrow = function () {
			var _p1 = maybeIsClosed;
			if (_p1.ctor === 'Nothing') {
				return _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow('');
			} else {
				if (_p1._0 === true) {
					return _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow('▸');
				} else {
					return _elm_lang$virtual_dom$VirtualDom_Expando$makeArrow('▾');
				}
			}
		}();
		var _p2 = maybeKey;
		if (_p2.ctor === 'Nothing') {
			return {ctor: '::', _0: arrow, _1: description};
		} else {
			return {
				ctor: '::',
				_0: arrow,
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Expando$purple,
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p2._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' = '),
						_1: description
					}
				}
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTinyRecord = F3(
	function (length, starter, entries) {
		var _p3 = entries;
		if (_p3.ctor === '[]') {
			return {
				ctor: '_Tuple2',
				_0: length + 1,
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('}'),
					_1: {ctor: '[]'}
				}
			};
		} else {
			var _p5 = _p3._0;
			var nextLength = (length + _elm_lang$core$String$length(_p5)) + 1;
			if (_elm_lang$core$Native_Utils.cmp(nextLength, 18) > 0) {
				return {
					ctor: '_Tuple2',
					_0: length + 2,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('…}'),
						_1: {ctor: '[]'}
					}
				};
			} else {
				var _p4 = A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTinyRecord, nextLength, ',', _p3._1);
				var finalLength = _p4._0;
				var otherNodes = _p4._1;
				return {
					ctor: '_Tuple2',
					_0: finalLength,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$purple,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p5),
									_1: {ctor: '[]'}
								}),
							_1: otherNodes
						}
					}
				};
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$elideMiddle = function (str) {
	return (_elm_lang$core$Native_Utils.cmp(
		_elm_lang$core$String$length(str),
		18) < 1) ? str : A2(
		_elm_lang$core$Basics_ops['++'],
		A2(_elm_lang$core$String$left, 8, str),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'...',
			A2(_elm_lang$core$String$right, 8, str)));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp = function (str) {
	return {
		ctor: '_Tuple2',
		_0: _elm_lang$core$String$length(str),
		_1: {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(str),
			_1: {ctor: '[]'}
		}
	};
};
var _elm_lang$virtual_dom$VirtualDom_Expando$updateIndex = F3(
	function (n, func, list) {
		var _p6 = list;
		if (_p6.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p8 = _p6._1;
			var _p7 = _p6._0;
			return (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) ? {
				ctor: '::',
				_0: func(_p7),
				_1: _p8
			} : {
				ctor: '::',
				_0: _p7,
				_1: A3(_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex, n - 1, func, _p8)
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$seqTypeToString = F2(
	function (n, seqType) {
		var _p9 = seqType;
		switch (_p9.ctor) {
			case 'ListSeq':
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'List(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(n),
						')'));
			case 'SetSeq':
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'Set(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(n),
						')'));
			default:
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'Array(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(n),
						')'));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTiny = function (value) {
	var _p10 = value;
	switch (_p10.ctor) {
		case 'S':
			var str = _elm_lang$virtual_dom$VirtualDom_Expando$elideMiddle(_p10._0);
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$String$length(str),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Expando$red,
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(str),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			};
		case 'Primitive':
			var _p11 = _p10._0;
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$String$length(_p11),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Expando$blue,
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p11),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			};
		case 'Sequence':
			return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
				A2(
					_elm_lang$virtual_dom$VirtualDom_Expando$seqTypeToString,
					_elm_lang$core$List$length(_p10._2),
					_p10._0));
		case 'Dictionary':
			return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'Dict(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(
							_elm_lang$core$List$length(_p10._1)),
						')')));
		case 'Record':
			return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecord(_p10._1);
		default:
			if (_p10._2.ctor === '[]') {
				return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
					A2(_elm_lang$core$Maybe$withDefault, 'Unit', _p10._0));
			} else {
				return _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyHelp(
					function () {
						var _p12 = _p10._0;
						if (_p12.ctor === 'Nothing') {
							return A2(
								_elm_lang$core$Basics_ops['++'],
								'Tuple(',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(
										_elm_lang$core$List$length(_p10._2)),
									')'));
						} else {
							return A2(_elm_lang$core$Basics_ops['++'], _p12._0, ' …');
						}
					}());
			}
	}
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecord = function (record) {
	return _elm_lang$core$Dict$isEmpty(record) ? {
		ctor: '_Tuple2',
		_0: 2,
		_1: {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('{}'),
			_1: {ctor: '[]'}
		}
	} : A3(
		_elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecordHelp,
		0,
		'{ ',
		_elm_lang$core$Dict$toList(record));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecordHelp = F3(
	function (length, starter, entries) {
		var _p13 = entries;
		if (_p13.ctor === '[]') {
			return {
				ctor: '_Tuple2',
				_0: length + 2,
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' }'),
					_1: {ctor: '[]'}
				}
			};
		} else {
			var _p16 = _p13._0._0;
			var _p14 = _elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTiny(_p13._0._1);
			var valueLen = _p14._0;
			var valueNodes = _p14._1;
			var fieldLen = _elm_lang$core$String$length(_p16);
			var newLength = ((length + fieldLen) + valueLen) + 5;
			if (_elm_lang$core$Native_Utils.cmp(newLength, 60) > 0) {
				return {
					ctor: '_Tuple2',
					_0: length + 4,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', … }'),
						_1: {ctor: '[]'}
					}
				};
			} else {
				var _p15 = A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecordHelp, newLength, ', ', _p13._1);
				var finalLength = _p15._0;
				var otherNodes = _p15._1;
				return {
					ctor: '_Tuple2',
					_0: finalLength,
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$purple,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p16),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' = '),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$virtual_dom$VirtualDom_Helpers$span,
										{ctor: '[]'},
										valueNodes),
									_1: otherNodes
								}
							}
						}
					}
				};
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTiny = function (value) {
	var _p17 = value;
	if (_p17.ctor === 'Record') {
		return A3(
			_elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTinyRecord,
			0,
			'{',
			_elm_lang$core$Dict$keys(_p17._1));
	} else {
		return _elm_lang$virtual_dom$VirtualDom_Expando$viewTiny(value);
	}
};
var _elm_lang$virtual_dom$VirtualDom_Expando$Constructor = F3(
	function (a, b, c) {
		return {ctor: 'Constructor', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Record = F2(
	function (a, b) {
		return {ctor: 'Record', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Dictionary = F2(
	function (a, b) {
		return {ctor: 'Dictionary', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Sequence = F3(
	function (a, b, c) {
		return {ctor: 'Sequence', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$initHelp = F2(
	function (isOuter, expando) {
		var _p18 = expando;
		switch (_p18.ctor) {
			case 'S':
				return expando;
			case 'Primitive':
				return expando;
			case 'Sequence':
				var _p20 = _p18._0;
				var _p19 = _p18._2;
				return isOuter ? A3(
					_elm_lang$virtual_dom$VirtualDom_Expando$Sequence,
					_p20,
					false,
					A2(
						_elm_lang$core$List$map,
						_elm_lang$virtual_dom$VirtualDom_Expando$initHelp(false),
						_p19)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(_p19),
					8) < 1) ? A3(_elm_lang$virtual_dom$VirtualDom_Expando$Sequence, _p20, false, _p19) : expando);
			case 'Dictionary':
				var _p23 = _p18._1;
				return isOuter ? A2(
					_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary,
					false,
					A2(
						_elm_lang$core$List$map,
						function (_p21) {
							var _p22 = _p21;
							return {
								ctor: '_Tuple2',
								_0: _p22._0,
								_1: A2(_elm_lang$virtual_dom$VirtualDom_Expando$initHelp, false, _p22._1)
							};
						},
						_p23)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(_p23),
					8) < 1) ? A2(_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary, false, _p23) : expando);
			case 'Record':
				var _p25 = _p18._1;
				return isOuter ? A2(
					_elm_lang$virtual_dom$VirtualDom_Expando$Record,
					false,
					A2(
						_elm_lang$core$Dict$map,
						F2(
							function (_p24, v) {
								return A2(_elm_lang$virtual_dom$VirtualDom_Expando$initHelp, false, v);
							}),
						_p25)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$Dict$size(_p25),
					4) < 1) ? A2(_elm_lang$virtual_dom$VirtualDom_Expando$Record, false, _p25) : expando);
			default:
				var _p27 = _p18._0;
				var _p26 = _p18._2;
				return isOuter ? A3(
					_elm_lang$virtual_dom$VirtualDom_Expando$Constructor,
					_p27,
					false,
					A2(
						_elm_lang$core$List$map,
						_elm_lang$virtual_dom$VirtualDom_Expando$initHelp(false),
						_p26)) : ((_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$List$length(_p26),
					4) < 1) ? A3(_elm_lang$virtual_dom$VirtualDom_Expando$Constructor, _p27, false, _p26) : expando);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$init = function (value) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Expando$initHelp,
		true,
		_elm_lang$virtual_dom$Native_Debug.init(value));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp = F2(
	function (old, $new) {
		var _p28 = {ctor: '_Tuple2', _0: old, _1: $new};
		_v12_6:
		do {
			if (_p28.ctor === '_Tuple2') {
				switch (_p28._1.ctor) {
					case 'S':
						return $new;
					case 'Primitive':
						return $new;
					case 'Sequence':
						if (_p28._0.ctor === 'Sequence') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Sequence,
								_p28._1._0,
								_p28._0._1,
								A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp, _p28._0._2, _p28._1._2));
						} else {
							break _v12_6;
						}
					case 'Dictionary':
						if (_p28._0.ctor === 'Dictionary') {
							return A2(_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary, _p28._0._0, _p28._1._1);
						} else {
							break _v12_6;
						}
					case 'Record':
						if (_p28._0.ctor === 'Record') {
							return A2(
								_elm_lang$virtual_dom$VirtualDom_Expando$Record,
								_p28._0._0,
								A2(
									_elm_lang$core$Dict$map,
									_elm_lang$virtual_dom$VirtualDom_Expando$mergeDictHelp(_p28._0._1),
									_p28._1._1));
						} else {
							break _v12_6;
						}
					default:
						if (_p28._0.ctor === 'Constructor') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Constructor,
								_p28._1._0,
								_p28._0._1,
								A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp, _p28._0._2, _p28._1._2));
						} else {
							break _v12_6;
						}
				}
			} else {
				break _v12_6;
			}
		} while(false);
		return $new;
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$mergeDictHelp = F3(
	function (oldDict, key, value) {
		var _p29 = A2(_elm_lang$core$Dict$get, key, oldDict);
		if (_p29.ctor === 'Nothing') {
			return value;
		} else {
			return A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp, _p29._0, value);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp = F2(
	function (olds, news) {
		var _p30 = {ctor: '_Tuple2', _0: olds, _1: news};
		if (_p30._0.ctor === '[]') {
			return news;
		} else {
			if (_p30._1.ctor === '[]') {
				return news;
			} else {
				return {
					ctor: '::',
					_0: A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp, _p30._0._0, _p30._1._0),
					_1: A2(_elm_lang$virtual_dom$VirtualDom_Expando$mergeListHelp, _p30._0._1, _p30._1._1)
				};
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$merge = F2(
	function (value, expando) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Expando$mergeHelp,
			expando,
			_elm_lang$virtual_dom$Native_Debug.init(value));
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$update = F2(
	function (msg, value) {
		var _p31 = value;
		switch (_p31.ctor) {
			case 'S':
				return _elm_lang$core$Native_Utils.crashCase(
					'VirtualDom.Expando',
					{
						start: {line: 168, column: 3},
						end: {line: 235, column: 50}
					},
					_p31)('No messages for primitives');
			case 'Primitive':
				return _elm_lang$core$Native_Utils.crashCase(
					'VirtualDom.Expando',
					{
						start: {line: 168, column: 3},
						end: {line: 235, column: 50}
					},
					_p31)('No messages for primitives');
			case 'Sequence':
				var _p39 = _p31._2;
				var _p38 = _p31._0;
				var _p37 = _p31._1;
				var _p34 = msg;
				switch (_p34.ctor) {
					case 'Toggle':
						return A3(_elm_lang$virtual_dom$VirtualDom_Expando$Sequence, _p38, !_p37, _p39);
					case 'Index':
						if (_p34._0.ctor === 'None') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Sequence,
								_p38,
								_p37,
								A3(
									_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
									_p34._1,
									_elm_lang$virtual_dom$VirtualDom_Expando$update(_p34._2),
									_p39));
						} else {
							return _elm_lang$core$Native_Utils.crashCase(
								'VirtualDom.Expando',
								{
									start: {line: 176, column: 7},
									end: {line: 188, column: 46}
								},
								_p34)('No redirected indexes on sequences');
						}
					default:
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 176, column: 7},
								end: {line: 188, column: 46}
							},
							_p34)('No field on sequences');
				}
			case 'Dictionary':
				var _p51 = _p31._1;
				var _p50 = _p31._0;
				var _p40 = msg;
				switch (_p40.ctor) {
					case 'Toggle':
						return A2(_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary, !_p50, _p51);
					case 'Index':
						var _p48 = _p40._2;
						var _p47 = _p40._1;
						var _p41 = _p40._0;
						switch (_p41.ctor) {
							case 'None':
								return _elm_lang$core$Native_Utils.crashCase(
									'VirtualDom.Expando',
									{
										start: {line: 196, column: 11},
										end: {line: 206, column: 81}
									},
									_p41)('must have redirect for dictionaries');
							case 'Key':
								return A2(
									_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary,
									_p50,
									A3(
										_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
										_p47,
										function (_p43) {
											var _p44 = _p43;
											return {
												ctor: '_Tuple2',
												_0: A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, _p48, _p44._0),
												_1: _p44._1
											};
										},
										_p51));
							default:
								return A2(
									_elm_lang$virtual_dom$VirtualDom_Expando$Dictionary,
									_p50,
									A3(
										_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
										_p47,
										function (_p45) {
											var _p46 = _p45;
											return {
												ctor: '_Tuple2',
												_0: _p46._0,
												_1: A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, _p48, _p46._1)
											};
										},
										_p51));
						}
					default:
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 191, column: 7},
								end: {line: 209, column: 50}
							},
							_p40)('no field for dictionaries');
				}
			case 'Record':
				var _p55 = _p31._1;
				var _p54 = _p31._0;
				var _p52 = msg;
				switch (_p52.ctor) {
					case 'Toggle':
						return A2(_elm_lang$virtual_dom$VirtualDom_Expando$Record, !_p54, _p55);
					case 'Index':
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 212, column: 7},
								end: {line: 220, column: 77}
							},
							_p52)('No index for records');
					default:
						return A2(
							_elm_lang$virtual_dom$VirtualDom_Expando$Record,
							_p54,
							A3(
								_elm_lang$core$Dict$update,
								_p52._0,
								_elm_lang$virtual_dom$VirtualDom_Expando$updateField(_p52._1),
								_p55));
				}
			default:
				var _p61 = _p31._2;
				var _p60 = _p31._0;
				var _p59 = _p31._1;
				var _p56 = msg;
				switch (_p56.ctor) {
					case 'Toggle':
						return A3(_elm_lang$virtual_dom$VirtualDom_Expando$Constructor, _p60, !_p59, _p61);
					case 'Index':
						if (_p56._0.ctor === 'None') {
							return A3(
								_elm_lang$virtual_dom$VirtualDom_Expando$Constructor,
								_p60,
								_p59,
								A3(
									_elm_lang$virtual_dom$VirtualDom_Expando$updateIndex,
									_p56._1,
									_elm_lang$virtual_dom$VirtualDom_Expando$update(_p56._2),
									_p61));
						} else {
							return _elm_lang$core$Native_Utils.crashCase(
								'VirtualDom.Expando',
								{
									start: {line: 223, column: 7},
									end: {line: 235, column: 50}
								},
								_p56)('No redirected indexes on sequences');
						}
					default:
						return _elm_lang$core$Native_Utils.crashCase(
							'VirtualDom.Expando',
							{
								start: {line: 223, column: 7},
								end: {line: 235, column: 50}
							},
							_p56)('No field for constructors');
				}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$updateField = F2(
	function (msg, maybeExpando) {
		var _p62 = maybeExpando;
		if (_p62.ctor === 'Nothing') {
			return _elm_lang$core$Native_Utils.crashCase(
				'VirtualDom.Expando',
				{
					start: {line: 253, column: 3},
					end: {line: 258, column: 32}
				},
				_p62)('key does not exist');
		} else {
			return _elm_lang$core$Maybe$Just(
				A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, msg, _p62._0));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Primitive = function (a) {
	return {ctor: 'Primitive', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Expando$S = function (a) {
	return {ctor: 'S', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Expando$ArraySeq = {ctor: 'ArraySeq'};
var _elm_lang$virtual_dom$VirtualDom_Expando$SetSeq = {ctor: 'SetSeq'};
var _elm_lang$virtual_dom$VirtualDom_Expando$ListSeq = {ctor: 'ListSeq'};
var _elm_lang$virtual_dom$VirtualDom_Expando$Field = F2(
	function (a, b) {
		return {ctor: 'Field', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Index = F3(
	function (a, b, c) {
		return {ctor: 'Index', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$Toggle = {ctor: 'Toggle'};
var _elm_lang$virtual_dom$VirtualDom_Expando$Value = {ctor: 'Value'};
var _elm_lang$virtual_dom$VirtualDom_Expando$Key = {ctor: 'Key'};
var _elm_lang$virtual_dom$VirtualDom_Expando$None = {ctor: 'None'};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorEntry = F2(
	function (index, value) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$map,
			A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, index),
			A2(
				_elm_lang$virtual_dom$VirtualDom_Expando$view,
				_elm_lang$core$Maybe$Just(
					_elm_lang$core$Basics$toString(index)),
				value));
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$view = F2(
	function (maybeKey, expando) {
		var _p64 = expando;
		switch (_p64.ctor) {
			case 'S':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Nothing,
						{
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$red,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p64._0),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}));
			case 'Primitive':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Nothing,
						{
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Expando$blue,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p64._0),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}));
			case 'Sequence':
				return A4(_elm_lang$virtual_dom$VirtualDom_Expando$viewSequence, maybeKey, _p64._0, _p64._1, _p64._2);
			case 'Dictionary':
				return A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewDictionary, maybeKey, _p64._0, _p64._1);
			case 'Record':
				return A3(_elm_lang$virtual_dom$VirtualDom_Expando$viewRecord, maybeKey, _p64._0, _p64._1);
			default:
				return A4(_elm_lang$virtual_dom$VirtualDom_Expando$viewConstructor, maybeKey, _p64._0, _p64._1, _p64._2);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructor = F4(
	function (maybeKey, maybeName, isClosed, valueList) {
		var _p65 = function () {
			var _p66 = valueList;
			if (_p66.ctor === '[]') {
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Maybe$Nothing,
					_1: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$div,
						{ctor: '[]'},
						{ctor: '[]'})
				};
			} else {
				if (_p66._1.ctor === '[]') {
					var _p67 = _p66._0;
					switch (_p67.ctor) {
						case 'S':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Nothing,
								_1: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'})
							};
						case 'Primitive':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Nothing,
								_1: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'})
							};
						case 'Sequence':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewSequenceOpen(_p67._2))
							};
						case 'Dictionary':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryOpen(_p67._1))
							};
						case 'Record':
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewRecordOpen(_p67._1))
							};
						default:
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Maybe$Just(isClosed),
								_1: isClosed ? A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$div,
									{ctor: '[]'},
									{ctor: '[]'}) : A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$None, 0),
									_elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorOpen(_p67._2))
							};
					}
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(isClosed),
						_1: isClosed ? A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$div,
							{ctor: '[]'},
							{ctor: '[]'}) : _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorOpen(valueList)
					};
				}
			}
		}();
		var maybeIsClosed = _p65._0;
		var openHtml = _p65._1;
		var tinyArgs = A2(
			_elm_lang$core$List$map,
			function (_p68) {
				return _elm_lang$core$Tuple$second(
					_elm_lang$virtual_dom$VirtualDom_Expando$viewExtraTiny(_p68));
			},
			valueList);
		var description = function () {
			var _p69 = {ctor: '_Tuple2', _0: maybeName, _1: tinyArgs};
			if (_p69._0.ctor === 'Nothing') {
				if (_p69._1.ctor === '[]') {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('()'),
						_1: {ctor: '[]'}
					};
				} else {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('( '),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{ctor: '[]'},
								_p69._1._0),
							_1: A3(
								_elm_lang$core$List$foldr,
								F2(
									function (args, rest) {
										return {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', '),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$virtual_dom$VirtualDom_Helpers$span,
													{ctor: '[]'},
													args),
												_1: rest
											}
										};
									}),
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' )'),
									_1: {ctor: '[]'}
								},
								_p69._1._1)
						}
					};
				}
			} else {
				if (_p69._1.ctor === '[]') {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p69._0._0),
						_1: {ctor: '[]'}
					};
				} else {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
							A2(_elm_lang$core$Basics_ops['++'], _p69._0._0, ' ')),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$span,
								{ctor: '[]'},
								_p69._1._0),
							_1: A3(
								_elm_lang$core$List$foldr,
								F2(
									function (args, rest) {
										return {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' '),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$virtual_dom$VirtualDom_Helpers$span,
													{ctor: '[]'},
													args),
												_1: rest
											}
										};
									}),
								{ctor: '[]'},
								_p69._1._1)
						}
					};
				}
			}
		}();
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter, maybeKey, maybeIsClosed, description)),
				_1: {
					ctor: '::',
					_0: openHtml,
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorOpen = function (valueList) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(_elm_lang$core$List$indexedMap, _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorEntry, valueList));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryOpen = function (keyValuePairs) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(_elm_lang$core$List$indexedMap, _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryEntry, keyValuePairs));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryEntry = F2(
	function (index, _p70) {
		var _p71 = _p70;
		var _p74 = _p71._1;
		var _p73 = _p71._0;
		var _p72 = _p73;
		switch (_p72.ctor) {
			case 'S':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$map,
					A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Value, index),
					A2(
						_elm_lang$virtual_dom$VirtualDom_Expando$view,
						_elm_lang$core$Maybe$Just(_p72._0),
						_p74));
			case 'Primitive':
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$map,
					A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Value, index),
					A2(
						_elm_lang$virtual_dom$VirtualDom_Expando$view,
						_elm_lang$core$Maybe$Just(_p72._0),
						_p74));
			default:
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$map,
							A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Key, index),
							A2(
								_elm_lang$virtual_dom$VirtualDom_Expando$view,
								_elm_lang$core$Maybe$Just('key'),
								_p73)),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$map,
								A2(_elm_lang$virtual_dom$VirtualDom_Expando$Index, _elm_lang$virtual_dom$VirtualDom_Expando$Value, index),
								A2(
									_elm_lang$virtual_dom$VirtualDom_Expando$view,
									_elm_lang$core$Maybe$Just('value'),
									_p74)),
							_1: {ctor: '[]'}
						}
					});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewRecordOpen = function (record) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(
			_elm_lang$core$List$map,
			_elm_lang$virtual_dom$VirtualDom_Expando$viewRecordEntry,
			_elm_lang$core$Dict$toList(record)));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewRecordEntry = function (_p75) {
	var _p76 = _p75;
	var _p77 = _p76._0;
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$map,
		_elm_lang$virtual_dom$VirtualDom_Expando$Field(_p77),
		A2(
			_elm_lang$virtual_dom$VirtualDom_Expando$view,
			_elm_lang$core$Maybe$Just(_p77),
			_p76._1));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewSequenceOpen = function (values) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{ctor: '[]'},
		A2(_elm_lang$core$List$indexedMap, _elm_lang$virtual_dom$VirtualDom_Expando$viewConstructorEntry, values));
};
var _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionary = F3(
	function (maybeKey, isClosed, keyValuePairs) {
		var starter = A2(
			_elm_lang$core$Basics_ops['++'],
			'Dict(',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(
					_elm_lang$core$List$length(keyValuePairs)),
				')'));
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Just(isClosed),
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
							_1: {ctor: '[]'}
						})),
				_1: {
					ctor: '::',
					_0: isClosed ? _elm_lang$virtual_dom$VirtualDom_Helpers$text('') : _elm_lang$virtual_dom$VirtualDom_Expando$viewDictionaryOpen(keyValuePairs),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewRecord = F3(
	function (maybeKey, isClosed, record) {
		var _p78 = isClosed ? {
			ctor: '_Tuple3',
			_0: _elm_lang$core$Tuple$second(
				_elm_lang$virtual_dom$VirtualDom_Expando$viewTinyRecord(record)),
			_1: _elm_lang$virtual_dom$VirtualDom_Helpers$text(''),
			_2: _elm_lang$virtual_dom$VirtualDom_Helpers$text('')
		} : {
			ctor: '_Tuple3',
			_0: {
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('{'),
				_1: {ctor: '[]'}
			},
			_1: _elm_lang$virtual_dom$VirtualDom_Expando$viewRecordOpen(record),
			_2: A2(
				_elm_lang$virtual_dom$VirtualDom_Helpers$div,
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(
						_elm_lang$core$Maybe$Just(
							{ctor: '_Tuple0'})),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('}'),
					_1: {ctor: '[]'}
				})
		};
		var start = _p78._0;
		var middle = _p78._1;
		var end = _p78._2;
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Just(isClosed),
						start)),
				_1: {
					ctor: '::',
					_0: middle,
					_1: {
						ctor: '::',
						_0: end,
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Expando$viewSequence = F4(
	function (maybeKey, seqType, isClosed, valueList) {
		var starter = A2(
			_elm_lang$virtual_dom$VirtualDom_Expando$seqTypeToString,
			_elm_lang$core$List$length(valueList),
			seqType);
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Expando$leftPad(maybeKey),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Expando$Toggle),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$virtual_dom$VirtualDom_Expando$lineStarter,
						maybeKey,
						_elm_lang$core$Maybe$Just(isClosed),
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(starter),
							_1: {ctor: '[]'}
						})),
				_1: {
					ctor: '::',
					_0: isClosed ? _elm_lang$virtual_dom$VirtualDom_Helpers$text('') : _elm_lang$virtual_dom$VirtualDom_Expando$viewSequenceOpen(valueList),
					_1: {ctor: '[]'}
				}
			});
	});

var _elm_lang$virtual_dom$VirtualDom_Report$some = function (list) {
	return !_elm_lang$core$List$isEmpty(list);
};
var _elm_lang$virtual_dom$VirtualDom_Report$TagChanges = F4(
	function (a, b, c, d) {
		return {removed: a, changed: b, added: c, argsMatch: d};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$emptyTagChanges = function (argsMatch) {
	return A4(
		_elm_lang$virtual_dom$VirtualDom_Report$TagChanges,
		{ctor: '[]'},
		{ctor: '[]'},
		{ctor: '[]'},
		argsMatch);
};
var _elm_lang$virtual_dom$VirtualDom_Report$hasTagChanges = function (tagChanges) {
	return _elm_lang$core$Native_Utils.eq(
		tagChanges,
		A4(
			_elm_lang$virtual_dom$VirtualDom_Report$TagChanges,
			{ctor: '[]'},
			{ctor: '[]'},
			{ctor: '[]'},
			true));
};
var _elm_lang$virtual_dom$VirtualDom_Report$SomethingChanged = function (a) {
	return {ctor: 'SomethingChanged', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Report$MessageChanged = F2(
	function (a, b) {
		return {ctor: 'MessageChanged', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$VersionChanged = F2(
	function (a, b) {
		return {ctor: 'VersionChanged', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$CorruptHistory = {ctor: 'CorruptHistory'};
var _elm_lang$virtual_dom$VirtualDom_Report$UnionChange = F2(
	function (a, b) {
		return {ctor: 'UnionChange', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Report$AliasChange = function (a) {
	return {ctor: 'AliasChange', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Report$Fine = {ctor: 'Fine'};
var _elm_lang$virtual_dom$VirtualDom_Report$Risky = {ctor: 'Risky'};
var _elm_lang$virtual_dom$VirtualDom_Report$Impossible = {ctor: 'Impossible'};
var _elm_lang$virtual_dom$VirtualDom_Report$worstCase = F2(
	function (status, statusList) {
		worstCase:
		while (true) {
			var _p0 = statusList;
			if (_p0.ctor === '[]') {
				return status;
			} else {
				switch (_p0._0.ctor) {
					case 'Impossible':
						return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
					case 'Risky':
						var _v1 = _elm_lang$virtual_dom$VirtualDom_Report$Risky,
							_v2 = _p0._1;
						status = _v1;
						statusList = _v2;
						continue worstCase;
					default:
						var _v3 = status,
							_v4 = _p0._1;
						status = _v3;
						statusList = _v4;
						continue worstCase;
				}
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Report$evaluateChange = function (change) {
	var _p1 = change;
	if (_p1.ctor === 'AliasChange') {
		return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
	} else {
		return ((!_p1._1.argsMatch) || (_elm_lang$virtual_dom$VirtualDom_Report$some(_p1._1.changed) || _elm_lang$virtual_dom$VirtualDom_Report$some(_p1._1.removed))) ? _elm_lang$virtual_dom$VirtualDom_Report$Impossible : (_elm_lang$virtual_dom$VirtualDom_Report$some(_p1._1.added) ? _elm_lang$virtual_dom$VirtualDom_Report$Risky : _elm_lang$virtual_dom$VirtualDom_Report$Fine);
	}
};
var _elm_lang$virtual_dom$VirtualDom_Report$evaluate = function (report) {
	var _p2 = report;
	switch (_p2.ctor) {
		case 'CorruptHistory':
			return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
		case 'VersionChanged':
			return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
		case 'MessageChanged':
			return _elm_lang$virtual_dom$VirtualDom_Report$Impossible;
		default:
			return A2(
				_elm_lang$virtual_dom$VirtualDom_Report$worstCase,
				_elm_lang$virtual_dom$VirtualDom_Report$Fine,
				A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Report$evaluateChange, _p2._0));
	}
};

var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict = F2(
	function (f, dict) {
		return _elm_lang$core$Json_Encode$object(
			_elm_lang$core$Dict$toList(
				A2(
					_elm_lang$core$Dict$map,
					F2(
						function (key, value) {
							return f(value);
						}),
					dict)));
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeUnion = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'args',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p1.args))
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'tags',
					_1: A2(
						_elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict,
						function (_p2) {
							return _elm_lang$core$Json_Encode$list(
								A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p2));
						},
						_p1.tags)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeAlias = function (_p3) {
	var _p4 = _p3;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'args',
				_1: _elm_lang$core$Json_Encode$list(
					A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, _p4.args))
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'type',
					_1: _elm_lang$core$Json_Encode$string(_p4.tipe)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeTypes = function (_p5) {
	var _p6 = _p5;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'message',
				_1: _elm_lang$core$Json_Encode$string(_p6.message)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'aliases',
					_1: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict, _elm_lang$virtual_dom$VirtualDom_Metadata$encodeAlias, _p6.aliases)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'unions',
						_1: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$encodeDict, _elm_lang$virtual_dom$VirtualDom_Metadata$encodeUnion, _p6.unions)
					},
					_1: {ctor: '[]'}
				}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encodeVersions = function (_p7) {
	var _p8 = _p7;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'elm',
				_1: _elm_lang$core$Json_Encode$string(_p8.elm)
			},
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$encode = function (_p9) {
	var _p10 = _p9;
	return _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'versions',
				_1: _elm_lang$virtual_dom$VirtualDom_Metadata$encodeVersions(_p10.versions)
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'types',
					_1: _elm_lang$virtual_dom$VirtualDom_Metadata$encodeTypes(_p10.types)
				},
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkTag = F4(
	function (tag, old, $new, changes) {
		return _elm_lang$core$Native_Utils.eq(old, $new) ? changes : _elm_lang$core$Native_Utils.update(
			changes,
			{
				changed: {ctor: '::', _0: tag, _1: changes.changed}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$addTag = F3(
	function (tag, _p11, changes) {
		return _elm_lang$core$Native_Utils.update(
			changes,
			{
				added: {ctor: '::', _0: tag, _1: changes.added}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$removeTag = F3(
	function (tag, _p12, changes) {
		return _elm_lang$core$Native_Utils.update(
			changes,
			{
				removed: {ctor: '::', _0: tag, _1: changes.removed}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkUnion = F4(
	function (name, old, $new, changes) {
		var tagChanges = A6(
			_elm_lang$core$Dict$merge,
			_elm_lang$virtual_dom$VirtualDom_Metadata$removeTag,
			_elm_lang$virtual_dom$VirtualDom_Metadata$checkTag,
			_elm_lang$virtual_dom$VirtualDom_Metadata$addTag,
			old.tags,
			$new.tags,
			_elm_lang$virtual_dom$VirtualDom_Report$emptyTagChanges(
				_elm_lang$core$Native_Utils.eq(old.args, $new.args)));
		return _elm_lang$virtual_dom$VirtualDom_Report$hasTagChanges(tagChanges) ? changes : {
			ctor: '::',
			_0: A2(_elm_lang$virtual_dom$VirtualDom_Report$UnionChange, name, tagChanges),
			_1: changes
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkAlias = F4(
	function (name, old, $new, changes) {
		return (_elm_lang$core$Native_Utils.eq(old.tipe, $new.tipe) && _elm_lang$core$Native_Utils.eq(old.args, $new.args)) ? changes : {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Report$AliasChange(name),
			_1: changes
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$ignore = F3(
	function (key, value, report) {
		return report;
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$checkTypes = F2(
	function (old, $new) {
		return (!_elm_lang$core$Native_Utils.eq(old.message, $new.message)) ? A2(_elm_lang$virtual_dom$VirtualDom_Report$MessageChanged, old.message, $new.message) : _elm_lang$virtual_dom$VirtualDom_Report$SomethingChanged(
			A6(
				_elm_lang$core$Dict$merge,
				_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
				_elm_lang$virtual_dom$VirtualDom_Metadata$checkUnion,
				_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
				old.unions,
				$new.unions,
				A6(
					_elm_lang$core$Dict$merge,
					_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
					_elm_lang$virtual_dom$VirtualDom_Metadata$checkAlias,
					_elm_lang$virtual_dom$VirtualDom_Metadata$ignore,
					old.aliases,
					$new.aliases,
					{ctor: '[]'})));
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$check = F2(
	function (old, $new) {
		return (!_elm_lang$core$Native_Utils.eq(old.versions.elm, $new.versions.elm)) ? A2(_elm_lang$virtual_dom$VirtualDom_Report$VersionChanged, old.versions.elm, $new.versions.elm) : A2(_elm_lang$virtual_dom$VirtualDom_Metadata$checkTypes, old.types, $new.types);
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$hasProblem = F2(
	function (tipe, _p13) {
		var _p14 = _p13;
		return A2(_elm_lang$core$String$contains, _p14._1, tipe) ? _elm_lang$core$Maybe$Just(_p14._0) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$Metadata = F2(
	function (a, b) {
		return {versions: a, types: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$Versions = function (a) {
	return {elm: a};
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeVersions = A2(
	_elm_lang$core$Json_Decode$map,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Versions,
	A2(_elm_lang$core$Json_Decode$field, 'elm', _elm_lang$core$Json_Decode$string));
var _elm_lang$virtual_dom$VirtualDom_Metadata$Types = F3(
	function (a, b, c) {
		return {message: a, aliases: b, unions: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$Alias = F2(
	function (a, b) {
		return {args: a, tipe: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeAlias = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Alias,
	A2(
		_elm_lang$core$Json_Decode$field,
		'args',
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string)),
	A2(_elm_lang$core$Json_Decode$field, 'type', _elm_lang$core$Json_Decode$string));
var _elm_lang$virtual_dom$VirtualDom_Metadata$Union = F2(
	function (a, b) {
		return {args: a, tags: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeUnion = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Union,
	A2(
		_elm_lang$core$Json_Decode$field,
		'args',
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string)),
	A2(
		_elm_lang$core$Json_Decode$field,
		'tags',
		_elm_lang$core$Json_Decode$dict(
			_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string))));
var _elm_lang$virtual_dom$VirtualDom_Metadata$decodeTypes = A4(
	_elm_lang$core$Json_Decode$map3,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Types,
	A2(_elm_lang$core$Json_Decode$field, 'message', _elm_lang$core$Json_Decode$string),
	A2(
		_elm_lang$core$Json_Decode$field,
		'aliases',
		_elm_lang$core$Json_Decode$dict(_elm_lang$virtual_dom$VirtualDom_Metadata$decodeAlias)),
	A2(
		_elm_lang$core$Json_Decode$field,
		'unions',
		_elm_lang$core$Json_Decode$dict(_elm_lang$virtual_dom$VirtualDom_Metadata$decodeUnion)));
var _elm_lang$virtual_dom$VirtualDom_Metadata$decoder = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$virtual_dom$VirtualDom_Metadata$Metadata,
	A2(_elm_lang$core$Json_Decode$field, 'versions', _elm_lang$virtual_dom$VirtualDom_Metadata$decodeVersions),
	A2(_elm_lang$core$Json_Decode$field, 'types', _elm_lang$virtual_dom$VirtualDom_Metadata$decodeTypes));
var _elm_lang$virtual_dom$VirtualDom_Metadata$Error = F2(
	function (a, b) {
		return {message: a, problems: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$ProblemType = F2(
	function (a, b) {
		return {name: a, problems: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$VirtualDom = {ctor: 'VirtualDom'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Program = {ctor: 'Program'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Request = {ctor: 'Request'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Socket = {ctor: 'Socket'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Process = {ctor: 'Process'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Task = {ctor: 'Task'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Decoder = {ctor: 'Decoder'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$Function = {ctor: 'Function'};
var _elm_lang$virtual_dom$VirtualDom_Metadata$problemTable = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Function, _1: '->'},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Decoder, _1: 'Json.Decode.Decoder'},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Task, _1: 'Task.Task'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Process, _1: 'Process.Id'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Socket, _1: 'WebSocket.LowLevel.WebSocket'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Request, _1: 'Http.Request'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$Program, _1: 'Platform.Program'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$VirtualDom, _1: 'VirtualDom.Node'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: _elm_lang$virtual_dom$VirtualDom_Metadata$VirtualDom, _1: 'VirtualDom.Attribute'},
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$findProblems = function (tipe) {
	return A2(
		_elm_lang$core$List$filterMap,
		_elm_lang$virtual_dom$VirtualDom_Metadata$hasProblem(tipe),
		_elm_lang$virtual_dom$VirtualDom_Metadata$problemTable);
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$collectBadAliases = F3(
	function (name, _p15, list) {
		var _p16 = _p15;
		var _p17 = _elm_lang$virtual_dom$VirtualDom_Metadata$findProblems(_p16.tipe);
		if (_p17.ctor === '[]') {
			return list;
		} else {
			return {
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$ProblemType, name, _p17),
				_1: list
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$collectBadUnions = F3(
	function (name, _p18, list) {
		var _p19 = _p18;
		var _p20 = A2(
			_elm_lang$core$List$concatMap,
			_elm_lang$virtual_dom$VirtualDom_Metadata$findProblems,
			_elm_lang$core$List$concat(
				_elm_lang$core$Dict$values(_p19.tags)));
		if (_p20.ctor === '[]') {
			return list;
		} else {
			return {
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Metadata$ProblemType, name, _p20),
				_1: list
			};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Metadata$isPortable = function (_p21) {
	var _p22 = _p21;
	var _p24 = _p22.types;
	var badAliases = A3(
		_elm_lang$core$Dict$foldl,
		_elm_lang$virtual_dom$VirtualDom_Metadata$collectBadAliases,
		{ctor: '[]'},
		_p24.aliases);
	var _p23 = A3(_elm_lang$core$Dict$foldl, _elm_lang$virtual_dom$VirtualDom_Metadata$collectBadUnions, badAliases, _p24.unions);
	if (_p23.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			A2(_elm_lang$virtual_dom$VirtualDom_Metadata$Error, _p24.message, _p23));
	}
};
var _elm_lang$virtual_dom$VirtualDom_Metadata$decode = function (value) {
	var _p25 = A2(_elm_lang$core$Json_Decode$decodeValue, _elm_lang$virtual_dom$VirtualDom_Metadata$decoder, value);
	if (_p25.ctor === 'Err') {
		return _elm_lang$core$Native_Utils.crashCase(
			'VirtualDom.Metadata',
			{
				start: {line: 229, column: 3},
				end: {line: 239, column: 20}
			},
			_p25)('Compiler is generating bad metadata. Report this at <https://github.com/elm-lang/virtual-dom/issues>.');
	} else {
		var _p28 = _p25._0;
		var _p27 = _elm_lang$virtual_dom$VirtualDom_Metadata$isPortable(_p28);
		if (_p27.ctor === 'Nothing') {
			return _elm_lang$core$Result$Ok(_p28);
		} else {
			return _elm_lang$core$Result$Err(_p27._0);
		}
	}
};

var _elm_lang$virtual_dom$VirtualDom_History$viewMessage = F3(
	function (currentIndex, index, msg) {
		var messageName = _elm_lang$virtual_dom$Native_Debug.messageToString(msg);
		var className = _elm_lang$core$Native_Utils.eq(currentIndex, index) ? 'messages-entry messages-entry-selected' : 'messages-entry';
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class(className),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$on,
						'click',
						_elm_lang$core$Json_Decode$succeed(index)),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$span,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('messages-entry-content'),
						_1: {
							ctor: '::',
							_0: A2(_elm_lang$virtual_dom$VirtualDom_Helpers$attribute, 'title', messageName),
							_1: {ctor: '[]'}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(messageName),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('messages-entry-index'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
								_elm_lang$core$Basics$toString(index)),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_History$consMsg = F3(
	function (currentIndex, msg, _p0) {
		var _p1 = _p0;
		var _p2 = _p1._0;
		return {
			ctor: '_Tuple2',
			_0: _p2 - 1,
			_1: {
				ctor: '::',
				_0: A4(_elm_lang$virtual_dom$VirtualDom_Helpers$lazy3, _elm_lang$virtual_dom$VirtualDom_History$viewMessage, currentIndex, _p2, msg),
				_1: _p1._1
			}
		};
	});
var _elm_lang$virtual_dom$VirtualDom_History$viewSnapshot = F3(
	function (currentIndex, index, _p3) {
		var _p4 = _p3;
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{ctor: '[]'},
			_elm_lang$core$Tuple$second(
				A3(
					_elm_lang$core$Array$foldl,
					_elm_lang$virtual_dom$VirtualDom_History$consMsg(currentIndex),
					{
						ctor: '_Tuple2',
						_0: index - 1,
						_1: {ctor: '[]'}
					},
					_p4.messages)));
	});
var _elm_lang$virtual_dom$VirtualDom_History$undone = function (getResult) {
	var _p5 = getResult;
	if (_p5.ctor === 'Done') {
		return {ctor: '_Tuple2', _0: _p5._1, _1: _p5._0};
	} else {
		return _elm_lang$core$Native_Utils.crashCase(
			'VirtualDom.History',
			{
				start: {line: 195, column: 3},
				end: {line: 200, column: 39}
			},
			_p5)('Bug in History.get');
	}
};
var _elm_lang$virtual_dom$VirtualDom_History$elmToJs = _elm_lang$virtual_dom$Native_Debug.unsafeCoerce;
var _elm_lang$virtual_dom$VirtualDom_History$encodeHelp = F2(
	function (snapshot, allMessages) {
		return A3(
			_elm_lang$core$Array$foldl,
			F2(
				function (elm, msgs) {
					return {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_History$elmToJs(elm),
						_1: msgs
					};
				}),
			allMessages,
			snapshot.messages);
	});
var _elm_lang$virtual_dom$VirtualDom_History$encode = function (_p7) {
	var _p8 = _p7;
	var recentJson = A2(
		_elm_lang$core$List$map,
		_elm_lang$virtual_dom$VirtualDom_History$elmToJs,
		_elm_lang$core$List$reverse(_p8.recent.messages));
	return _elm_lang$core$Json_Encode$list(
		A3(_elm_lang$core$Array$foldr, _elm_lang$virtual_dom$VirtualDom_History$encodeHelp, recentJson, _p8.snapshots));
};
var _elm_lang$virtual_dom$VirtualDom_History$jsToElm = _elm_lang$virtual_dom$Native_Debug.unsafeCoerce;
var _elm_lang$virtual_dom$VirtualDom_History$initialModel = function (_p9) {
	var _p10 = _p9;
	var _p11 = A2(_elm_lang$core$Array$get, 0, _p10.snapshots);
	if (_p11.ctor === 'Just') {
		return _p11._0.model;
	} else {
		return _p10.recent.model;
	}
};
var _elm_lang$virtual_dom$VirtualDom_History$size = function (history) {
	return history.numMessages;
};
var _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize = 64;
var _elm_lang$virtual_dom$VirtualDom_History$consSnapshot = F3(
	function (currentIndex, snapshot, _p12) {
		var _p13 = _p12;
		var _p14 = _p13._0;
		var nextIndex = _p14 - _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize;
		var currentIndexHelp = ((_elm_lang$core$Native_Utils.cmp(nextIndex, currentIndex) < 1) && (_elm_lang$core$Native_Utils.cmp(currentIndex, _p14) < 0)) ? currentIndex : -1;
		return {
			ctor: '_Tuple2',
			_0: _p14 - _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize,
			_1: {
				ctor: '::',
				_0: A4(_elm_lang$virtual_dom$VirtualDom_Helpers$lazy3, _elm_lang$virtual_dom$VirtualDom_History$viewSnapshot, currentIndexHelp, _p14, snapshot),
				_1: _p13._1
			}
		};
	});
var _elm_lang$virtual_dom$VirtualDom_History$viewSnapshots = F2(
	function (currentIndex, snapshots) {
		var highIndex = _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize * _elm_lang$core$Array$length(snapshots);
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{ctor: '[]'},
			_elm_lang$core$Tuple$second(
				A3(
					_elm_lang$core$Array$foldr,
					_elm_lang$virtual_dom$VirtualDom_History$consSnapshot(currentIndex),
					{
						ctor: '_Tuple2',
						_0: highIndex,
						_1: {ctor: '[]'}
					},
					snapshots)));
	});
var _elm_lang$virtual_dom$VirtualDom_History$view = F2(
	function (maybeIndex, _p15) {
		var _p16 = _p15;
		var _p17 = function () {
			var _p18 = maybeIndex;
			if (_p18.ctor === 'Nothing') {
				return {ctor: '_Tuple2', _0: -1, _1: 'debugger-sidebar-messages'};
			} else {
				return {ctor: '_Tuple2', _0: _p18._0, _1: 'debugger-sidebar-messages-paused'};
			}
		}();
		var index = _p17._0;
		var className = _p17._1;
		var oldStuff = A3(_elm_lang$virtual_dom$VirtualDom_Helpers$lazy2, _elm_lang$virtual_dom$VirtualDom_History$viewSnapshots, index, _p16.snapshots);
		var newStuff = _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				_elm_lang$virtual_dom$VirtualDom_History$consMsg(index),
				{
					ctor: '_Tuple2',
					_0: _p16.numMessages - 1,
					_1: {ctor: '[]'}
				},
				_p16.recent.messages));
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class(className),
				_1: {ctor: '[]'}
			},
			{ctor: '::', _0: oldStuff, _1: newStuff});
	});
var _elm_lang$virtual_dom$VirtualDom_History$History = F3(
	function (a, b, c) {
		return {snapshots: a, recent: b, numMessages: c};
	});
var _elm_lang$virtual_dom$VirtualDom_History$RecentHistory = F3(
	function (a, b, c) {
		return {model: a, messages: b, numMessages: c};
	});
var _elm_lang$virtual_dom$VirtualDom_History$empty = function (model) {
	return A3(
		_elm_lang$virtual_dom$VirtualDom_History$History,
		_elm_lang$core$Array$empty,
		A3(
			_elm_lang$virtual_dom$VirtualDom_History$RecentHistory,
			model,
			{ctor: '[]'},
			0),
		0);
};
var _elm_lang$virtual_dom$VirtualDom_History$Snapshot = F2(
	function (a, b) {
		return {model: a, messages: b};
	});
var _elm_lang$virtual_dom$VirtualDom_History$addRecent = F3(
	function (msg, newModel, _p19) {
		var _p20 = _p19;
		var _p23 = _p20.numMessages;
		var _p22 = _p20.model;
		var _p21 = _p20.messages;
		return _elm_lang$core$Native_Utils.eq(_p23, _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize) ? {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Maybe$Just(
				A2(
					_elm_lang$virtual_dom$VirtualDom_History$Snapshot,
					_p22,
					_elm_lang$core$Array$fromList(_p21))),
			_1: A3(
				_elm_lang$virtual_dom$VirtualDom_History$RecentHistory,
				newModel,
				{
					ctor: '::',
					_0: msg,
					_1: {ctor: '[]'}
				},
				1)
		} : {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Maybe$Nothing,
			_1: A3(
				_elm_lang$virtual_dom$VirtualDom_History$RecentHistory,
				_p22,
				{ctor: '::', _0: msg, _1: _p21},
				_p23 + 1)
		};
	});
var _elm_lang$virtual_dom$VirtualDom_History$add = F3(
	function (msg, model, _p24) {
		var _p25 = _p24;
		var _p28 = _p25.snapshots;
		var _p27 = _p25.numMessages;
		var _p26 = A3(_elm_lang$virtual_dom$VirtualDom_History$addRecent, msg, model, _p25.recent);
		if (_p26._0.ctor === 'Just') {
			return A3(
				_elm_lang$virtual_dom$VirtualDom_History$History,
				A2(_elm_lang$core$Array$push, _p26._0._0, _p28),
				_p26._1,
				_p27 + 1);
		} else {
			return A3(_elm_lang$virtual_dom$VirtualDom_History$History, _p28, _p26._1, _p27 + 1);
		}
	});
var _elm_lang$virtual_dom$VirtualDom_History$decoder = F2(
	function (initialModel, update) {
		var addMessage = F2(
			function (rawMsg, _p29) {
				var _p30 = _p29;
				var _p31 = _p30._0;
				var msg = _elm_lang$virtual_dom$VirtualDom_History$jsToElm(rawMsg);
				return {
					ctor: '_Tuple2',
					_0: A2(update, msg, _p31),
					_1: A3(_elm_lang$virtual_dom$VirtualDom_History$add, msg, _p31, _p30._1)
				};
			});
		var updateModel = function (rawMsgs) {
			return A3(
				_elm_lang$core$List$foldl,
				addMessage,
				{
					ctor: '_Tuple2',
					_0: initialModel,
					_1: _elm_lang$virtual_dom$VirtualDom_History$empty(initialModel)
				},
				rawMsgs);
		};
		return A2(
			_elm_lang$core$Json_Decode$map,
			updateModel,
			_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$value));
	});
var _elm_lang$virtual_dom$VirtualDom_History$Done = F2(
	function (a, b) {
		return {ctor: 'Done', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_History$Stepping = F2(
	function (a, b) {
		return {ctor: 'Stepping', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_History$getHelp = F3(
	function (update, msg, getResult) {
		var _p32 = getResult;
		if (_p32.ctor === 'Done') {
			return getResult;
		} else {
			var _p34 = _p32._0;
			var _p33 = _p32._1;
			return _elm_lang$core$Native_Utils.eq(_p34, 0) ? A2(
				_elm_lang$virtual_dom$VirtualDom_History$Done,
				msg,
				_elm_lang$core$Tuple$first(
					A2(update, msg, _p33))) : A2(
				_elm_lang$virtual_dom$VirtualDom_History$Stepping,
				_p34 - 1,
				_elm_lang$core$Tuple$first(
					A2(update, msg, _p33)));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_History$get = F3(
	function (update, index, _p35) {
		var _p36 = _p35;
		var _p39 = _p36.recent;
		var snapshotMax = _p36.numMessages - _p39.numMessages;
		if (_elm_lang$core$Native_Utils.cmp(index, snapshotMax) > -1) {
			return _elm_lang$virtual_dom$VirtualDom_History$undone(
				A3(
					_elm_lang$core$List$foldr,
					_elm_lang$virtual_dom$VirtualDom_History$getHelp(update),
					A2(_elm_lang$virtual_dom$VirtualDom_History$Stepping, index - snapshotMax, _p39.model),
					_p39.messages));
		} else {
			var _p37 = A2(_elm_lang$core$Array$get, (index / _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize) | 0, _p36.snapshots);
			if (_p37.ctor === 'Nothing') {
				return _elm_lang$core$Native_Utils.crashCase(
					'VirtualDom.History',
					{
						start: {line: 165, column: 7},
						end: {line: 171, column: 95}
					},
					_p37)('UI should only let you ask for real indexes!');
			} else {
				return _elm_lang$virtual_dom$VirtualDom_History$undone(
					A3(
						_elm_lang$core$Array$foldr,
						_elm_lang$virtual_dom$VirtualDom_History$getHelp(update),
						A2(
							_elm_lang$virtual_dom$VirtualDom_History$Stepping,
							A2(_elm_lang$core$Basics$rem, index, _elm_lang$virtual_dom$VirtualDom_History$maxSnapshotSize),
							_p37._0.model),
						_p37._0.messages));
			}
		}
	});

var _elm_lang$virtual_dom$VirtualDom_Overlay$styles = A3(
	_elm_lang$virtual_dom$VirtualDom_Helpers$node,
	'style',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('\n\n.elm-overlay {\n  position: fixed;\n  top: 0;\n  left: 0;\n  width: 100%;\n  height: 100%;\n  color: white;\n  pointer-events: none;\n  font-family: \'Trebuchet MS\', \'Lucida Grande\', \'Bitstream Vera Sans\', \'Helvetica Neue\', sans-serif;\n}\n\n.elm-overlay-resume {\n  width: 100%;\n  height: 100%;\n  cursor: pointer;\n  text-align: center;\n  pointer-events: auto;\n  background-color: rgba(200, 200, 200, 0.7);\n}\n\n.elm-overlay-resume-words {\n  position: absolute;\n  top: calc(50% - 40px);\n  font-size: 80px;\n  line-height: 80px;\n  height: 80px;\n  width: 100%;\n}\n\n.elm-mini-controls {\n  position: fixed;\n  bottom: 0;\n  right: 6px;\n  border-radius: 4px;\n  background-color: rgb(61, 61, 61);\n  font-family: monospace;\n  pointer-events: auto;\n}\n\n.elm-mini-controls-button {\n  padding: 6px;\n  cursor: pointer;\n  text-align: center;\n  min-width: 24ch;\n}\n\n.elm-mini-controls-import-export {\n  padding: 4px 0;\n  font-size: 0.8em;\n  text-align: center;\n  background-color: rgb(50, 50, 50);\n}\n\n.elm-overlay-message {\n  position: absolute;\n  width: 600px;\n  height: 100%;\n  padding-left: calc(50% - 300px);\n  padding-right: calc(50% - 300px);\n  background-color: rgba(200, 200, 200, 0.7);\n  pointer-events: auto;\n}\n\n.elm-overlay-message-title {\n  font-size: 36px;\n  height: 80px;\n  background-color: rgb(50, 50, 50);\n  padding-left: 22px;\n  vertical-align: middle;\n  line-height: 80px;\n}\n\n.elm-overlay-message-details {\n  padding: 8px 20px;\n  overflow-y: auto;\n  max-height: calc(100% - 156px);\n  background-color: rgb(61, 61, 61);\n}\n\n.elm-overlay-message-details-type {\n  font-size: 1.5em;\n}\n\n.elm-overlay-message-details ul {\n  list-style-type: none;\n  padding-left: 20px;\n}\n\n.elm-overlay-message-details ul ul {\n  list-style-type: disc;\n  padding-left: 2em;\n}\n\n.elm-overlay-message-details li {\n  margin: 8px 0;\n}\n\n.elm-overlay-message-buttons {\n  height: 60px;\n  line-height: 60px;\n  text-align: right;\n  background-color: rgb(50, 50, 50);\n}\n\n.elm-overlay-message-buttons button {\n  margin-right: 20px;\n}\n\n'),
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$button = F2(
	function (msg, label) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$span,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(msg),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'cursor', _1: 'pointer'},
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(label),
				_1: {ctor: '[]'}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewImportExport = F3(
	function (props, importMsg, exportMsg) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			props,
			{
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$button, importMsg, 'Import'),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' / '),
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$button, exportMsg, 'Export'),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewMiniControls = F2(
	function (config, numMsgs) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-mini-controls'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(config.open),
						_1: {
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-mini-controls-button'),
							_1: {ctor: '[]'}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'Explore History (',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(numMsgs),
									')'))),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Overlay$viewImportExport,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-mini-controls-import-export'),
							_1: {ctor: '[]'}
						},
						config.importHistory,
						config.exportHistory),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$addCommas = function (items) {
	var _p0 = items;
	if (_p0.ctor === '[]') {
		return '';
	} else {
		if (_p0._1.ctor === '[]') {
			return _p0._0;
		} else {
			if (_p0._1._1.ctor === '[]') {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_p0._0,
					A2(_elm_lang$core$Basics_ops['++'], ' and ', _p0._1._0));
			} else {
				return A2(
					_elm_lang$core$String$join,
					', ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_p0._1,
						{
							ctor: '::',
							_0: A2(_elm_lang$core$Basics_ops['++'], ' and ', _p0._0),
							_1: {ctor: '[]'}
						}));
			}
		}
	}
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$problemToString = function (problem) {
	var _p1 = problem;
	switch (_p1.ctor) {
		case 'Function':
			return 'functions';
		case 'Decoder':
			return 'JSON decoders';
		case 'Task':
			return 'tasks';
		case 'Process':
			return 'processes';
		case 'Socket':
			return 'web sockets';
		case 'Request':
			return 'HTTP requests';
		case 'Program':
			return 'programs';
		default:
			return 'virtual DOM values';
	}
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$goodNews2 = '\nfunction can pattern match on that data and call whatever functions, JSON\ndecoders, etc. you need. This makes the code much more explicit and easy to\nfollow for other readers (or you in a few months!)\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$goodNews1 = '\nThe good news is that having values like this in your message type is not\nso great in the long run. You are better off using simpler data, like\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode = function (name) {
	return A3(
		_elm_lang$virtual_dom$VirtualDom_Helpers$node,
		'code',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(name),
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewMention = F2(
	function (tags, verbed) {
		var _p2 = A2(
			_elm_lang$core$List$map,
			_elm_lang$virtual_dom$VirtualDom_Overlay$viewCode,
			_elm_lang$core$List$reverse(tags));
		if (_p2.ctor === '[]') {
			return _elm_lang$virtual_dom$VirtualDom_Helpers$text('');
		} else {
			if (_p2._1.ctor === '[]') {
				return A3(
					_elm_lang$virtual_dom$VirtualDom_Helpers$node,
					'li',
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(verbed),
						_1: {
							ctor: '::',
							_0: _p2._0,
							_1: {
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('.'),
								_1: {ctor: '[]'}
							}
						}
					});
			} else {
				if (_p2._1._1.ctor === '[]') {
					return A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'li',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(verbed),
							_1: {
								ctor: '::',
								_0: _p2._1._0,
								_1: {
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' and '),
									_1: {
										ctor: '::',
										_0: _p2._0,
										_1: {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('.'),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						});
				} else {
					return A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'li',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(verbed),
							_1: A2(
								_elm_lang$core$Basics_ops['++'],
								A2(
									_elm_lang$core$List$intersperse,
									_elm_lang$virtual_dom$VirtualDom_Helpers$text(', '),
									_elm_lang$core$List$reverse(_p2._1)),
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', and '),
									_1: {
										ctor: '::',
										_0: _p2._0,
										_1: {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('.'),
											_1: {ctor: '[]'}
										}
									}
								})
						});
				}
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewChange = function (change) {
	return A3(
		_elm_lang$virtual_dom$VirtualDom_Helpers$node,
		'li',
		{ctor: '[]'},
		function () {
			var _p3 = change;
			if (_p3.ctor === 'AliasChange') {
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-details-type'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p3._0),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			} else {
				return {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$span,
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-details-type'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p3._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A3(
							_elm_lang$virtual_dom$VirtualDom_Helpers$node,
							'ul',
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMention, _p3._1.removed, 'Removed '),
								_1: {
									ctor: '::',
									_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMention, _p3._1.changed, 'Changed '),
									_1: {
										ctor: '::',
										_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMention, _p3._1.added, 'Added '),
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {
							ctor: '::',
							_0: _p3._1.argsMatch ? _elm_lang$virtual_dom$VirtualDom_Helpers$text('') : _elm_lang$virtual_dom$VirtualDom_Helpers$text('This may be due to the fact that the type variable names changed.'),
							_1: {ctor: '[]'}
						}
					}
				};
			}
		}());
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewProblemType = function (_p4) {
	var _p5 = _p4;
	return A3(
		_elm_lang$virtual_dom$VirtualDom_Helpers$node,
		'li',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p5.name),
			_1: {
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
					A2(
						_elm_lang$core$Basics_ops['++'],
						' can contain ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$virtual_dom$VirtualDom_Overlay$addCommas(
								A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Overlay$problemToString, _p5.problems)),
							'.'))),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewBadMetadata = function (_p6) {
	var _p7 = _p6;
	return {
		ctor: '::',
		_0: A3(
			_elm_lang$virtual_dom$VirtualDom_Helpers$node,
			'p',
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('The '),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p7.message),
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' type of your program cannot be reliably serialized for history files.'),
						_1: {ctor: '[]'}
					}
				}
			}),
		_1: {
			ctor: '::',
			_0: A3(
				_elm_lang$virtual_dom$VirtualDom_Helpers$node,
				'p',
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Functions cannot be serialized, nor can values that contain functions. This is a problem in these places:'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A3(
					_elm_lang$virtual_dom$VirtualDom_Helpers$node,
					'ul',
					{ctor: '[]'},
					A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Overlay$viewProblemType, _p7.problems)),
				_1: {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'p',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_elm_lang$virtual_dom$VirtualDom_Overlay$goodNews1),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$a,
									{
										ctor: '::',
										_0: _elm_lang$virtual_dom$VirtualDom_Helpers$href('https://guide.elm-lang.org/types/union_types.html'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('union types'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(', in your messages. From there, your '),
									_1: {
										ctor: '::',
										_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode('update'),
										_1: {
											ctor: '::',
											_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_elm_lang$virtual_dom$VirtualDom_Overlay$goodNews2),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}),
					_1: {ctor: '[]'}
				}
			}
		}
	};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$explanationRisky = '\nThis history seems old. It will work with this program, but some\nmessages have been added since the history was created:\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$explanationBad = '\nThe messages in this history do not match the messages handled by your\nprogram. I noticed changes in the following types:\n';
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewReport = F2(
	function (isBad, report) {
		var _p8 = report;
		switch (_p8.ctor) {
			case 'CorruptHistory':
				return {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Looks like this history file is corrupt. I cannot understand it.'),
					_1: {ctor: '[]'}
				};
			case 'VersionChanged':
				return {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'This history was created with Elm ',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_p8._0,
								A2(
									_elm_lang$core$Basics_ops['++'],
									', but you are using Elm ',
									A2(_elm_lang$core$Basics_ops['++'], _p8._1, ' right now.'))))),
					_1: {ctor: '[]'}
				};
			case 'MessageChanged':
				return {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
						A2(_elm_lang$core$Basics_ops['++'], 'To import some other history, the overall message type must', ' be the same. The old history has ')),
					_1: {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p8._0),
						_1: {
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' messages, but the new program works with '),
							_1: {
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewCode(_p8._1),
								_1: {
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' messages.'),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				};
			default:
				return {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'p',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(
								isBad ? _elm_lang$virtual_dom$VirtualDom_Overlay$explanationBad : _elm_lang$virtual_dom$VirtualDom_Overlay$explanationRisky),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A3(
							_elm_lang$virtual_dom$VirtualDom_Helpers$node,
							'ul',
							{ctor: '[]'},
							A2(_elm_lang$core$List$map, _elm_lang$virtual_dom$VirtualDom_Overlay$viewChange, _p8._0)),
						_1: {ctor: '[]'}
					}
				};
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewResume = function (config) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-resume'),
			_1: {
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(config.resume),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$virtual_dom$VirtualDom_Helpers$div,
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-resume-words'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Click to Resume'),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$uploadDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}),
	A2(_elm_lang$core$Json_Decode$field, 'metadata', _elm_lang$virtual_dom$VirtualDom_Metadata$decoder),
	A2(_elm_lang$core$Json_Decode$field, 'history', _elm_lang$core$Json_Decode$value));
var _elm_lang$virtual_dom$VirtualDom_Overlay$close = F2(
	function (msg, state) {
		var _p9 = state;
		switch (_p9.ctor) {
			case 'None':
				return _elm_lang$core$Maybe$Nothing;
			case 'BadMetadata':
				return _elm_lang$core$Maybe$Nothing;
			case 'BadImport':
				return _elm_lang$core$Maybe$Nothing;
			default:
				var _p10 = msg;
				if (_p10.ctor === 'Cancel') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					return _elm_lang$core$Maybe$Just(_p9._1);
				}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$isBlocking = function (state) {
	var _p11 = state;
	if (_p11.ctor === 'None') {
		return false;
	} else {
		return true;
	}
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Config = F5(
	function (a, b, c, d, e) {
		return {resume: a, open: b, importHistory: c, exportHistory: d, wrap: e};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$RiskyImport = F2(
	function (a, b) {
		return {ctor: 'RiskyImport', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$BadImport = function (a) {
	return {ctor: 'BadImport', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$corruptImport = _elm_lang$virtual_dom$VirtualDom_Overlay$BadImport(_elm_lang$virtual_dom$VirtualDom_Report$CorruptHistory);
var _elm_lang$virtual_dom$VirtualDom_Overlay$assessImport = F2(
	function (metadata, jsonString) {
		var _p12 = A2(_elm_lang$core$Json_Decode$decodeString, _elm_lang$virtual_dom$VirtualDom_Overlay$uploadDecoder, jsonString);
		if (_p12.ctor === 'Err') {
			return _elm_lang$core$Result$Err(_elm_lang$virtual_dom$VirtualDom_Overlay$corruptImport);
		} else {
			var _p14 = _p12._0._1;
			var report = A2(_elm_lang$virtual_dom$VirtualDom_Metadata$check, _p12._0._0, metadata);
			var _p13 = _elm_lang$virtual_dom$VirtualDom_Report$evaluate(report);
			switch (_p13.ctor) {
				case 'Impossible':
					return _elm_lang$core$Result$Err(
						_elm_lang$virtual_dom$VirtualDom_Overlay$BadImport(report));
				case 'Risky':
					return _elm_lang$core$Result$Err(
						A2(_elm_lang$virtual_dom$VirtualDom_Overlay$RiskyImport, report, _p14));
				default:
					return _elm_lang$core$Result$Ok(_p14);
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$BadMetadata = function (a) {
	return {ctor: 'BadMetadata', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$badMetadata = _elm_lang$virtual_dom$VirtualDom_Overlay$BadMetadata;
var _elm_lang$virtual_dom$VirtualDom_Overlay$None = {ctor: 'None'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$none = _elm_lang$virtual_dom$VirtualDom_Overlay$None;
var _elm_lang$virtual_dom$VirtualDom_Overlay$Proceed = {ctor: 'Proceed'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Cancel = {ctor: 'Cancel'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewButtons = function (buttons) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-buttons'),
			_1: {ctor: '[]'}
		},
		function () {
			var _p15 = buttons;
			if (_p15.ctor === 'Accept') {
				return {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'button',
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Overlay$Proceed),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p15._0),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			} else {
				return {
					ctor: '::',
					_0: A3(
						_elm_lang$virtual_dom$VirtualDom_Helpers$node,
						'button',
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Overlay$Cancel),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p15._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A3(
							_elm_lang$virtual_dom$VirtualDom_Helpers$node,
							'button',
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Overlay$Proceed),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(_p15._1),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				};
			}
		}());
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Message = {ctor: 'Message'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage = F4(
	function (config, title, details, buttons) {
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$virtual_dom$VirtualDom_Overlay$Message,
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$div,
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-title'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(title),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$virtual_dom$VirtualDom_Helpers$div,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay-message-details'),
									_1: {ctor: '[]'}
								},
								details),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$virtual_dom$VirtualDom_Helpers$map,
									config.wrap,
									_elm_lang$virtual_dom$VirtualDom_Overlay$viewButtons(buttons)),
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {ctor: '[]'}
			}
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$Pause = {ctor: 'Pause'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Normal = {ctor: 'Normal'};
var _elm_lang$virtual_dom$VirtualDom_Overlay$Choose = F2(
	function (a, b) {
		return {ctor: 'Choose', _0: a, _1: b};
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$Accept = function (a) {
	return {ctor: 'Accept', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Overlay$viewHelp = F5(
	function (config, isPaused, isOpen, numMsgs, state) {
		var _p16 = state;
		switch (_p16.ctor) {
			case 'None':
				var miniControls = isOpen ? {ctor: '[]'} : {
					ctor: '::',
					_0: A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewMiniControls, config, numMsgs),
					_1: {ctor: '[]'}
				};
				return {
					ctor: '_Tuple2',
					_0: isPaused ? _elm_lang$virtual_dom$VirtualDom_Overlay$Pause : _elm_lang$virtual_dom$VirtualDom_Overlay$Normal,
					_1: (isPaused && (!isOpen)) ? {
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Overlay$viewResume(config),
						_1: miniControls
					} : miniControls
				};
			case 'BadMetadata':
				return A4(
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage,
					config,
					'Cannot use Import or Export',
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewBadMetadata(_p16._0),
					_elm_lang$virtual_dom$VirtualDom_Overlay$Accept('Ok'));
			case 'BadImport':
				return A4(
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage,
					config,
					'Cannot Import History',
					A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewReport, true, _p16._0),
					_elm_lang$virtual_dom$VirtualDom_Overlay$Accept('Ok'));
			default:
				return A4(
					_elm_lang$virtual_dom$VirtualDom_Overlay$viewMessage,
					config,
					'Warning',
					A2(_elm_lang$virtual_dom$VirtualDom_Overlay$viewReport, false, _p16._0),
					A2(_elm_lang$virtual_dom$VirtualDom_Overlay$Choose, 'Cancel', 'Import Anyway'));
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Overlay$view = F5(
	function (config, isPaused, isOpen, numMsgs, state) {
		var _p17 = A5(_elm_lang$virtual_dom$VirtualDom_Overlay$viewHelp, config, isPaused, isOpen, numMsgs, state);
		var block = _p17._0;
		var nodes = _p17._1;
		return {
			ctor: '_Tuple2',
			_0: block,
			_1: A2(
				_elm_lang$virtual_dom$VirtualDom_Helpers$div,
				{
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('elm-overlay'),
					_1: {ctor: '[]'}
				},
				{ctor: '::', _0: _elm_lang$virtual_dom$VirtualDom_Overlay$styles, _1: nodes})
		};
	});

var _elm_lang$virtual_dom$VirtualDom_Debug$styles = A3(
	_elm_lang$virtual_dom$VirtualDom_Helpers$node,
	'style',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('\n\nhtml {\n    overflow: hidden;\n    height: 100%;\n}\n\nbody {\n    height: 100%;\n    overflow: auto;\n}\n\n#debugger {\n  width: 100%\n  height: 100%;\n  font-family: monospace;\n}\n\n#values {\n  display: block;\n  float: left;\n  height: 100%;\n  width: calc(100% - 30ch);\n  margin: 0;\n  overflow: auto;\n  cursor: default;\n}\n\n.debugger-sidebar {\n  display: block;\n  float: left;\n  width: 30ch;\n  height: 100%;\n  color: white;\n  background-color: rgb(61, 61, 61);\n}\n\n.debugger-sidebar-controls {\n  width: 100%;\n  text-align: center;\n  background-color: rgb(50, 50, 50);\n}\n\n.debugger-sidebar-controls-import-export {\n  width: 100%;\n  height: 24px;\n  line-height: 24px;\n  font-size: 12px;\n}\n\n.debugger-sidebar-controls-resume {\n  width: 100%;\n  height: 30px;\n  line-height: 30px;\n  cursor: pointer;\n}\n\n.debugger-sidebar-controls-resume:hover {\n  background-color: rgb(41, 41, 41);\n}\n\n.debugger-sidebar-messages {\n  width: 100%;\n  overflow-y: auto;\n  height: calc(100% - 24px);\n}\n\n.debugger-sidebar-messages-paused {\n  width: 100%;\n  overflow-y: auto;\n  height: calc(100% - 54px);\n}\n\n.messages-entry {\n  cursor: pointer;\n  width: 100%;\n}\n\n.messages-entry:hover {\n  background-color: rgb(41, 41, 41);\n}\n\n.messages-entry-selected, .messages-entry-selected:hover {\n  background-color: rgb(10, 10, 10);\n}\n\n.messages-entry-content {\n  width: calc(100% - 7ch);\n  padding-top: 4px;\n  padding-bottom: 4px;\n  padding-left: 1ch;\n  text-overflow: ellipsis;\n  white-space: nowrap;\n  overflow: hidden;\n  display: inline-block;\n}\n\n.messages-entry-index {\n  color: #666;\n  width: 5ch;\n  padding-top: 4px;\n  padding-bottom: 4px;\n  padding-right: 1ch;\n  text-align: right;\n  display: block;\n  float: right;\n}\n\n'),
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$button = F2(
	function (msg, label) {
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$span,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(msg),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Helpers$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'cursor', _1: 'pointer'},
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(label),
				_1: {ctor: '[]'}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel = function (state) {
	var _p0 = state;
	if (_p0.ctor === 'Running') {
		return _p0._0;
	} else {
		return _p0._2;
	}
};
var _elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata = F2(
	function (model, func) {
		var _p1 = model.metadata;
		if (_p1.ctor === 'Ok') {
			return func(_p1._0);
		} else {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{
						overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$badMetadata(_p1._0)
					}),
				{ctor: '[]'});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$Model = F6(
	function (a, b, c, d, e, f) {
		return {history: a, state: b, expando: c, metadata: d, overlay: e, isDebuggerOpen: f};
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$Paused = F3(
	function (a, b, c) {
		return {ctor: 'Paused', _0: a, _1: b, _2: c};
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$Running = function (a) {
	return {ctor: 'Running', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$loadNewHistory = F3(
	function (rawHistory, userUpdate, model) {
		var pureUserUpdate = F2(
			function (msg, userModel) {
				return _elm_lang$core$Tuple$first(
					A2(userUpdate, msg, userModel));
			});
		var initialUserModel = _elm_lang$virtual_dom$VirtualDom_History$initialModel(model.history);
		var decoder = A2(_elm_lang$virtual_dom$VirtualDom_History$decoder, initialUserModel, pureUserUpdate);
		var _p2 = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, rawHistory);
		if (_p2.ctor === 'Err') {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$corruptImport}),
				{ctor: '[]'});
		} else {
			var _p3 = _p2._0._0;
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					model,
					{
						history: _p2._0._1,
						state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(_p3),
						expando: _elm_lang$virtual_dom$VirtualDom_Expando$init(_p3),
						overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$none
					}),
				{ctor: '[]'});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$OverlayMsg = function (a) {
	return {ctor: 'OverlayMsg', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$Upload = function (a) {
	return {ctor: 'Upload', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$upload = A2(_elm_lang$core$Task$perform, _elm_lang$virtual_dom$VirtualDom_Debug$Upload, _elm_lang$virtual_dom$Native_Debug.upload);
var _elm_lang$virtual_dom$VirtualDom_Debug$Export = {ctor: 'Export'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Import = {ctor: 'Import'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Down = {ctor: 'Down'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Up = {ctor: 'Up'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Close = {ctor: 'Close'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Open = {ctor: 'Open'};
var _elm_lang$virtual_dom$VirtualDom_Debug$Jump = function (a) {
	return {ctor: 'Jump', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$Resume = {ctor: 'Resume'};
var _elm_lang$virtual_dom$VirtualDom_Debug$overlayConfig = {resume: _elm_lang$virtual_dom$VirtualDom_Debug$Resume, open: _elm_lang$virtual_dom$VirtualDom_Debug$Open, importHistory: _elm_lang$virtual_dom$VirtualDom_Debug$Import, exportHistory: _elm_lang$virtual_dom$VirtualDom_Debug$Export, wrap: _elm_lang$virtual_dom$VirtualDom_Debug$OverlayMsg};
var _elm_lang$virtual_dom$VirtualDom_Debug$viewIn = function (_p4) {
	var _p5 = _p4;
	var isPaused = function () {
		var _p6 = _p5.state;
		if (_p6.ctor === 'Running') {
			return false;
		} else {
			return true;
		}
	}();
	return A5(
		_elm_lang$virtual_dom$VirtualDom_Overlay$view,
		_elm_lang$virtual_dom$VirtualDom_Debug$overlayConfig,
		isPaused,
		_p5.isDebuggerOpen,
		_elm_lang$virtual_dom$VirtualDom_History$size(_p5.history),
		_p5.overlay);
};
var _elm_lang$virtual_dom$VirtualDom_Debug$resumeButton = A2(
	_elm_lang$virtual_dom$VirtualDom_Helpers$div,
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$onClick(_elm_lang$virtual_dom$VirtualDom_Debug$Resume),
		_1: {
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar-controls-resume'),
			_1: {ctor: '[]'}
		}
	},
	{
		ctor: '::',
		_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text('Resume'),
		_1: {ctor: '[]'}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$viewResumeButton = function (maybeIndex) {
	var _p7 = maybeIndex;
	if (_p7.ctor === 'Nothing') {
		return _elm_lang$virtual_dom$VirtualDom_Helpers$text('');
	} else {
		return _elm_lang$virtual_dom$VirtualDom_Debug$resumeButton;
	}
};
var _elm_lang$virtual_dom$VirtualDom_Debug$playButton = function (maybeIndex) {
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar-controls'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Debug$viewResumeButton(maybeIndex),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$div,
					{
						ctor: '::',
						_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar-controls-import-export'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$button, _elm_lang$virtual_dom$VirtualDom_Debug$Import, 'Import'),
						_1: {
							ctor: '::',
							_0: _elm_lang$virtual_dom$VirtualDom_Helpers$text(' / '),
							_1: {
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$button, _elm_lang$virtual_dom$VirtualDom_Debug$Export, 'Export'),
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Debug$viewSidebar = F2(
	function (state, history) {
		var maybeIndex = function () {
			var _p8 = state;
			if (_p8.ctor === 'Running') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(_p8._0);
			}
		}();
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$div,
			{
				ctor: '::',
				_0: _elm_lang$virtual_dom$VirtualDom_Helpers$class('debugger-sidebar'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$virtual_dom$VirtualDom_Helpers$map,
					_elm_lang$virtual_dom$VirtualDom_Debug$Jump,
					A2(_elm_lang$virtual_dom$VirtualDom_History$view, maybeIndex, history)),
				_1: {
					ctor: '::',
					_0: _elm_lang$virtual_dom$VirtualDom_Debug$playButton(maybeIndex),
					_1: {ctor: '[]'}
				}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$ExpandoMsg = function (a) {
	return {ctor: 'ExpandoMsg', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$viewOut = function (_p9) {
	var _p10 = _p9;
	return A2(
		_elm_lang$virtual_dom$VirtualDom_Helpers$div,
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Helpers$id('debugger'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$virtual_dom$VirtualDom_Debug$styles,
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$viewSidebar, _p10.state, _p10.history),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$virtual_dom$VirtualDom_Helpers$map,
						_elm_lang$virtual_dom$VirtualDom_Debug$ExpandoMsg,
						A2(
							_elm_lang$virtual_dom$VirtualDom_Helpers$div,
							{
								ctor: '::',
								_0: _elm_lang$virtual_dom$VirtualDom_Helpers$id('values'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Expando$view, _elm_lang$core$Maybe$Nothing, _p10.expando),
								_1: {ctor: '[]'}
							})),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _elm_lang$virtual_dom$VirtualDom_Debug$UserMsg = function (a) {
	return {ctor: 'UserMsg', _0: a};
};
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapInit = F2(
	function (metadata, _p11) {
		var _p12 = _p11;
		var _p13 = _p12._0;
		return A2(
			_elm_lang$core$Platform_Cmd_ops['!'],
			{
				history: _elm_lang$virtual_dom$VirtualDom_History$empty(_p13),
				state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(_p13),
				expando: _elm_lang$virtual_dom$VirtualDom_Expando$init(_p13),
				metadata: _elm_lang$virtual_dom$VirtualDom_Metadata$decode(metadata),
				overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$none,
				isDebuggerOpen: false
			},
			{
				ctor: '::',
				_0: A2(_elm_lang$core$Platform_Cmd$map, _elm_lang$virtual_dom$VirtualDom_Debug$UserMsg, _p12._1),
				_1: {ctor: '[]'}
			});
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapSubs = F2(
	function (userSubscriptions, _p14) {
		var _p15 = _p14;
		return A2(
			_elm_lang$core$Platform_Sub$map,
			_elm_lang$virtual_dom$VirtualDom_Debug$UserMsg,
			userSubscriptions(
				_elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel(_p15.state)));
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapView = F2(
	function (userView, _p16) {
		var _p17 = _p16;
		var currentModel = function () {
			var _p18 = _p17.state;
			if (_p18.ctor === 'Running') {
				return _p18._0;
			} else {
				return _p18._1;
			}
		}();
		return A2(
			_elm_lang$virtual_dom$VirtualDom_Helpers$map,
			_elm_lang$virtual_dom$VirtualDom_Debug$UserMsg,
			userView(currentModel));
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$NoOp = {ctor: 'NoOp'};
var _elm_lang$virtual_dom$VirtualDom_Debug$download = F2(
	function (metadata, history) {
		var json = _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'metadata',
					_1: _elm_lang$virtual_dom$VirtualDom_Metadata$encode(metadata)
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'history',
						_1: _elm_lang$virtual_dom$VirtualDom_History$encode(history)
					},
					_1: {ctor: '[]'}
				}
			});
		var historyLength = _elm_lang$virtual_dom$VirtualDom_History$size(history);
		return A2(
			_elm_lang$core$Task$perform,
			function (_p19) {
				return _elm_lang$virtual_dom$VirtualDom_Debug$NoOp;
			},
			A2(_elm_lang$virtual_dom$Native_Debug.download, historyLength, json));
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$runIf = F2(
	function (bool, task) {
		return bool ? A2(
			_elm_lang$core$Task$perform,
			_elm_lang$core$Basics$always(_elm_lang$virtual_dom$VirtualDom_Debug$NoOp),
			task) : _elm_lang$core$Platform_Cmd$none;
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$updateUserMsg = F4(
	function (userUpdate, scrollTask, userMsg, _p20) {
		var _p21 = _p20;
		var _p25 = _p21.state;
		var _p24 = _p21;
		var userModel = _elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel(_p25);
		var newHistory = A3(_elm_lang$virtual_dom$VirtualDom_History$add, userMsg, userModel, _p21.history);
		var _p22 = A2(userUpdate, userMsg, userModel);
		var newUserModel = _p22._0;
		var userCmds = _p22._1;
		var commands = A2(_elm_lang$core$Platform_Cmd$map, _elm_lang$virtual_dom$VirtualDom_Debug$UserMsg, userCmds);
		var _p23 = _p25;
		if (_p23.ctor === 'Running') {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					_p24,
					{
						history: newHistory,
						state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(newUserModel),
						expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$merge, newUserModel, _p21.expando)
					}),
				{
					ctor: '::',
					_0: commands,
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$runIf, _p24.isDebuggerOpen, scrollTask),
						_1: {ctor: '[]'}
					}
				});
		} else {
			return A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_elm_lang$core$Native_Utils.update(
					_p24,
					{
						history: newHistory,
						state: A3(_elm_lang$virtual_dom$VirtualDom_Debug$Paused, _p23._0, _p23._1, newUserModel)
					}),
				{
					ctor: '::',
					_0: commands,
					_1: {ctor: '[]'}
				});
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapUpdate = F4(
	function (userUpdate, scrollTask, msg, model) {
		wrapUpdate:
		while (true) {
			var _p26 = msg;
			switch (_p26.ctor) {
				case 'NoOp':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						model,
						{ctor: '[]'});
				case 'UserMsg':
					return A4(_elm_lang$virtual_dom$VirtualDom_Debug$updateUserMsg, userUpdate, scrollTask, _p26._0, model);
				case 'ExpandoMsg':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{
								expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$update, _p26._0, model.expando)
							}),
						{ctor: '[]'});
				case 'Resume':
					var _p27 = model.state;
					if (_p27.ctor === 'Running') {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					} else {
						var _p28 = _p27._2;
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							_elm_lang$core$Native_Utils.update(
								model,
								{
									state: _elm_lang$virtual_dom$VirtualDom_Debug$Running(_p28),
									expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$merge, _p28, model.expando)
								}),
							{
								ctor: '::',
								_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$runIf, model.isDebuggerOpen, scrollTask),
								_1: {ctor: '[]'}
							});
					}
				case 'Jump':
					var _p30 = _p26._0;
					var _p29 = A3(_elm_lang$virtual_dom$VirtualDom_History$get, userUpdate, _p30, model.history);
					var indexModel = _p29._0;
					var indexMsg = _p29._1;
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{
								state: A3(
									_elm_lang$virtual_dom$VirtualDom_Debug$Paused,
									_p30,
									indexModel,
									_elm_lang$virtual_dom$VirtualDom_Debug$getLatestModel(model.state)),
								expando: A2(_elm_lang$virtual_dom$VirtualDom_Expando$merge, indexModel, model.expando)
							}),
						{ctor: '[]'});
				case 'Open':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{isDebuggerOpen: true}),
						{ctor: '[]'});
				case 'Close':
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{isDebuggerOpen: false}),
						{ctor: '[]'});
				case 'Up':
					var index = function () {
						var _p31 = model.state;
						if (_p31.ctor === 'Paused') {
							return _p31._0;
						} else {
							return _elm_lang$virtual_dom$VirtualDom_History$size(model.history);
						}
					}();
					if (_elm_lang$core$Native_Utils.cmp(index, 0) > 0) {
						var _v17 = userUpdate,
							_v18 = scrollTask,
							_v19 = _elm_lang$virtual_dom$VirtualDom_Debug$Jump(index - 1),
							_v20 = model;
						userUpdate = _v17;
						scrollTask = _v18;
						msg = _v19;
						model = _v20;
						continue wrapUpdate;
					} else {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					}
				case 'Down':
					var _p32 = model.state;
					if (_p32.ctor === 'Running') {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					} else {
						var _p33 = _p32._0;
						if (_elm_lang$core$Native_Utils.eq(
							_p33,
							_elm_lang$virtual_dom$VirtualDom_History$size(model.history) - 1)) {
							var _v22 = userUpdate,
								_v23 = scrollTask,
								_v24 = _elm_lang$virtual_dom$VirtualDom_Debug$Resume,
								_v25 = model;
							userUpdate = _v22;
							scrollTask = _v23;
							msg = _v24;
							model = _v25;
							continue wrapUpdate;
						} else {
							var _v26 = userUpdate,
								_v27 = scrollTask,
								_v28 = _elm_lang$virtual_dom$VirtualDom_Debug$Jump(_p33 + 1),
								_v29 = model;
							userUpdate = _v26;
							scrollTask = _v27;
							msg = _v28;
							model = _v29;
							continue wrapUpdate;
						}
					}
				case 'Import':
					return A2(
						_elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata,
						model,
						function (_p34) {
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								{
									ctor: '::',
									_0: _elm_lang$virtual_dom$VirtualDom_Debug$upload,
									_1: {ctor: '[]'}
								});
						});
				case 'Export':
					return A2(
						_elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata,
						model,
						function (metadata) {
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								{
									ctor: '::',
									_0: A2(_elm_lang$virtual_dom$VirtualDom_Debug$download, metadata, model.history),
									_1: {ctor: '[]'}
								});
						});
				case 'Upload':
					return A2(
						_elm_lang$virtual_dom$VirtualDom_Debug$withGoodMetadata,
						model,
						function (metadata) {
							var _p35 = A2(_elm_lang$virtual_dom$VirtualDom_Overlay$assessImport, metadata, _p26._0);
							if (_p35.ctor === 'Err') {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									_elm_lang$core$Native_Utils.update(
										model,
										{overlay: _p35._0}),
									{ctor: '[]'});
							} else {
								return A3(_elm_lang$virtual_dom$VirtualDom_Debug$loadNewHistory, _p35._0, userUpdate, model);
							}
						});
				default:
					var _p36 = A2(_elm_lang$virtual_dom$VirtualDom_Overlay$close, _p26._0, model.overlay);
					if (_p36.ctor === 'Nothing') {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							_elm_lang$core$Native_Utils.update(
								model,
								{overlay: _elm_lang$virtual_dom$VirtualDom_Overlay$none}),
							{ctor: '[]'});
					} else {
						return A3(_elm_lang$virtual_dom$VirtualDom_Debug$loadNewHistory, _p36._0, userUpdate, model);
					}
			}
		}
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrap = F2(
	function (metadata, _p37) {
		var _p38 = _p37;
		return {
			init: A2(_elm_lang$virtual_dom$VirtualDom_Debug$wrapInit, metadata, _p38.init),
			view: _elm_lang$virtual_dom$VirtualDom_Debug$wrapView(_p38.view),
			update: _elm_lang$virtual_dom$VirtualDom_Debug$wrapUpdate(_p38.update),
			viewIn: _elm_lang$virtual_dom$VirtualDom_Debug$viewIn,
			viewOut: _elm_lang$virtual_dom$VirtualDom_Debug$viewOut,
			subscriptions: _elm_lang$virtual_dom$VirtualDom_Debug$wrapSubs(_p38.subscriptions)
		};
	});
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags = F2(
	function (metadata, _p39) {
		var _p40 = _p39;
		return {
			init: function (flags) {
				return A2(
					_elm_lang$virtual_dom$VirtualDom_Debug$wrapInit,
					metadata,
					_p40.init(flags));
			},
			view: _elm_lang$virtual_dom$VirtualDom_Debug$wrapView(_p40.view),
			update: _elm_lang$virtual_dom$VirtualDom_Debug$wrapUpdate(_p40.update),
			viewIn: _elm_lang$virtual_dom$VirtualDom_Debug$viewIn,
			viewOut: _elm_lang$virtual_dom$VirtualDom_Debug$viewOut,
			subscriptions: _elm_lang$virtual_dom$VirtualDom_Debug$wrapSubs(_p40.subscriptions)
		};
	});

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$svg$Svg$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$svg$Svg$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$svg$Svg$svgNamespace = A2(
	_elm_lang$virtual_dom$VirtualDom$property,
	'namespace',
	_elm_lang$core$Json_Encode$string('http://www.w3.org/2000/svg'));
var _elm_lang$svg$Svg$node = F3(
	function (name, attributes, children) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$node,
			name,
			{ctor: '::', _0: _elm_lang$svg$Svg$svgNamespace, _1: attributes},
			children);
	});
var _elm_lang$svg$Svg$svg = _elm_lang$svg$Svg$node('svg');
var _elm_lang$svg$Svg$foreignObject = _elm_lang$svg$Svg$node('foreignObject');
var _elm_lang$svg$Svg$animate = _elm_lang$svg$Svg$node('animate');
var _elm_lang$svg$Svg$animateColor = _elm_lang$svg$Svg$node('animateColor');
var _elm_lang$svg$Svg$animateMotion = _elm_lang$svg$Svg$node('animateMotion');
var _elm_lang$svg$Svg$animateTransform = _elm_lang$svg$Svg$node('animateTransform');
var _elm_lang$svg$Svg$mpath = _elm_lang$svg$Svg$node('mpath');
var _elm_lang$svg$Svg$set = _elm_lang$svg$Svg$node('set');
var _elm_lang$svg$Svg$a = _elm_lang$svg$Svg$node('a');
var _elm_lang$svg$Svg$defs = _elm_lang$svg$Svg$node('defs');
var _elm_lang$svg$Svg$g = _elm_lang$svg$Svg$node('g');
var _elm_lang$svg$Svg$marker = _elm_lang$svg$Svg$node('marker');
var _elm_lang$svg$Svg$mask = _elm_lang$svg$Svg$node('mask');
var _elm_lang$svg$Svg$pattern = _elm_lang$svg$Svg$node('pattern');
var _elm_lang$svg$Svg$switch = _elm_lang$svg$Svg$node('switch');
var _elm_lang$svg$Svg$symbol = _elm_lang$svg$Svg$node('symbol');
var _elm_lang$svg$Svg$desc = _elm_lang$svg$Svg$node('desc');
var _elm_lang$svg$Svg$metadata = _elm_lang$svg$Svg$node('metadata');
var _elm_lang$svg$Svg$title = _elm_lang$svg$Svg$node('title');
var _elm_lang$svg$Svg$feBlend = _elm_lang$svg$Svg$node('feBlend');
var _elm_lang$svg$Svg$feColorMatrix = _elm_lang$svg$Svg$node('feColorMatrix');
var _elm_lang$svg$Svg$feComponentTransfer = _elm_lang$svg$Svg$node('feComponentTransfer');
var _elm_lang$svg$Svg$feComposite = _elm_lang$svg$Svg$node('feComposite');
var _elm_lang$svg$Svg$feConvolveMatrix = _elm_lang$svg$Svg$node('feConvolveMatrix');
var _elm_lang$svg$Svg$feDiffuseLighting = _elm_lang$svg$Svg$node('feDiffuseLighting');
var _elm_lang$svg$Svg$feDisplacementMap = _elm_lang$svg$Svg$node('feDisplacementMap');
var _elm_lang$svg$Svg$feFlood = _elm_lang$svg$Svg$node('feFlood');
var _elm_lang$svg$Svg$feFuncA = _elm_lang$svg$Svg$node('feFuncA');
var _elm_lang$svg$Svg$feFuncB = _elm_lang$svg$Svg$node('feFuncB');
var _elm_lang$svg$Svg$feFuncG = _elm_lang$svg$Svg$node('feFuncG');
var _elm_lang$svg$Svg$feFuncR = _elm_lang$svg$Svg$node('feFuncR');
var _elm_lang$svg$Svg$feGaussianBlur = _elm_lang$svg$Svg$node('feGaussianBlur');
var _elm_lang$svg$Svg$feImage = _elm_lang$svg$Svg$node('feImage');
var _elm_lang$svg$Svg$feMerge = _elm_lang$svg$Svg$node('feMerge');
var _elm_lang$svg$Svg$feMergeNode = _elm_lang$svg$Svg$node('feMergeNode');
var _elm_lang$svg$Svg$feMorphology = _elm_lang$svg$Svg$node('feMorphology');
var _elm_lang$svg$Svg$feOffset = _elm_lang$svg$Svg$node('feOffset');
var _elm_lang$svg$Svg$feSpecularLighting = _elm_lang$svg$Svg$node('feSpecularLighting');
var _elm_lang$svg$Svg$feTile = _elm_lang$svg$Svg$node('feTile');
var _elm_lang$svg$Svg$feTurbulence = _elm_lang$svg$Svg$node('feTurbulence');
var _elm_lang$svg$Svg$font = _elm_lang$svg$Svg$node('font');
var _elm_lang$svg$Svg$linearGradient = _elm_lang$svg$Svg$node('linearGradient');
var _elm_lang$svg$Svg$radialGradient = _elm_lang$svg$Svg$node('radialGradient');
var _elm_lang$svg$Svg$stop = _elm_lang$svg$Svg$node('stop');
var _elm_lang$svg$Svg$circle = _elm_lang$svg$Svg$node('circle');
var _elm_lang$svg$Svg$ellipse = _elm_lang$svg$Svg$node('ellipse');
var _elm_lang$svg$Svg$image = _elm_lang$svg$Svg$node('image');
var _elm_lang$svg$Svg$line = _elm_lang$svg$Svg$node('line');
var _elm_lang$svg$Svg$path = _elm_lang$svg$Svg$node('path');
var _elm_lang$svg$Svg$polygon = _elm_lang$svg$Svg$node('polygon');
var _elm_lang$svg$Svg$polyline = _elm_lang$svg$Svg$node('polyline');
var _elm_lang$svg$Svg$rect = _elm_lang$svg$Svg$node('rect');
var _elm_lang$svg$Svg$use = _elm_lang$svg$Svg$node('use');
var _elm_lang$svg$Svg$feDistantLight = _elm_lang$svg$Svg$node('feDistantLight');
var _elm_lang$svg$Svg$fePointLight = _elm_lang$svg$Svg$node('fePointLight');
var _elm_lang$svg$Svg$feSpotLight = _elm_lang$svg$Svg$node('feSpotLight');
var _elm_lang$svg$Svg$altGlyph = _elm_lang$svg$Svg$node('altGlyph');
var _elm_lang$svg$Svg$altGlyphDef = _elm_lang$svg$Svg$node('altGlyphDef');
var _elm_lang$svg$Svg$altGlyphItem = _elm_lang$svg$Svg$node('altGlyphItem');
var _elm_lang$svg$Svg$glyph = _elm_lang$svg$Svg$node('glyph');
var _elm_lang$svg$Svg$glyphRef = _elm_lang$svg$Svg$node('glyphRef');
var _elm_lang$svg$Svg$textPath = _elm_lang$svg$Svg$node('textPath');
var _elm_lang$svg$Svg$text_ = _elm_lang$svg$Svg$node('text');
var _elm_lang$svg$Svg$tref = _elm_lang$svg$Svg$node('tref');
var _elm_lang$svg$Svg$tspan = _elm_lang$svg$Svg$node('tspan');
var _elm_lang$svg$Svg$clipPath = _elm_lang$svg$Svg$node('clipPath');
var _elm_lang$svg$Svg$colorProfile = _elm_lang$svg$Svg$node('colorProfile');
var _elm_lang$svg$Svg$cursor = _elm_lang$svg$Svg$node('cursor');
var _elm_lang$svg$Svg$filter = _elm_lang$svg$Svg$node('filter');
var _elm_lang$svg$Svg$script = _elm_lang$svg$Svg$node('script');
var _elm_lang$svg$Svg$style = _elm_lang$svg$Svg$node('style');
var _elm_lang$svg$Svg$view = _elm_lang$svg$Svg$node('view');

var _elm_lang$svg$Svg_Attributes$writingMode = _elm_lang$virtual_dom$VirtualDom$attribute('writing-mode');
var _elm_lang$svg$Svg_Attributes$wordSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('word-spacing');
var _elm_lang$svg$Svg_Attributes$visibility = _elm_lang$virtual_dom$VirtualDom$attribute('visibility');
var _elm_lang$svg$Svg_Attributes$unicodeBidi = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-bidi');
var _elm_lang$svg$Svg_Attributes$textRendering = _elm_lang$virtual_dom$VirtualDom$attribute('text-rendering');
var _elm_lang$svg$Svg_Attributes$textDecoration = _elm_lang$virtual_dom$VirtualDom$attribute('text-decoration');
var _elm_lang$svg$Svg_Attributes$textAnchor = _elm_lang$virtual_dom$VirtualDom$attribute('text-anchor');
var _elm_lang$svg$Svg_Attributes$stroke = _elm_lang$virtual_dom$VirtualDom$attribute('stroke');
var _elm_lang$svg$Svg_Attributes$strokeWidth = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-width');
var _elm_lang$svg$Svg_Attributes$strokeOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-opacity');
var _elm_lang$svg$Svg_Attributes$strokeMiterlimit = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-miterlimit');
var _elm_lang$svg$Svg_Attributes$strokeLinejoin = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linejoin');
var _elm_lang$svg$Svg_Attributes$strokeLinecap = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linecap');
var _elm_lang$svg$Svg_Attributes$strokeDashoffset = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dashoffset');
var _elm_lang$svg$Svg_Attributes$strokeDasharray = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dasharray');
var _elm_lang$svg$Svg_Attributes$stopOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stop-opacity');
var _elm_lang$svg$Svg_Attributes$stopColor = _elm_lang$virtual_dom$VirtualDom$attribute('stop-color');
var _elm_lang$svg$Svg_Attributes$shapeRendering = _elm_lang$virtual_dom$VirtualDom$attribute('shape-rendering');
var _elm_lang$svg$Svg_Attributes$pointerEvents = _elm_lang$virtual_dom$VirtualDom$attribute('pointer-events');
var _elm_lang$svg$Svg_Attributes$overflow = _elm_lang$virtual_dom$VirtualDom$attribute('overflow');
var _elm_lang$svg$Svg_Attributes$opacity = _elm_lang$virtual_dom$VirtualDom$attribute('opacity');
var _elm_lang$svg$Svg_Attributes$mask = _elm_lang$virtual_dom$VirtualDom$attribute('mask');
var _elm_lang$svg$Svg_Attributes$markerStart = _elm_lang$virtual_dom$VirtualDom$attribute('marker-start');
var _elm_lang$svg$Svg_Attributes$markerMid = _elm_lang$virtual_dom$VirtualDom$attribute('marker-mid');
var _elm_lang$svg$Svg_Attributes$markerEnd = _elm_lang$virtual_dom$VirtualDom$attribute('marker-end');
var _elm_lang$svg$Svg_Attributes$lightingColor = _elm_lang$virtual_dom$VirtualDom$attribute('lighting-color');
var _elm_lang$svg$Svg_Attributes$letterSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('letter-spacing');
var _elm_lang$svg$Svg_Attributes$kerning = _elm_lang$virtual_dom$VirtualDom$attribute('kerning');
var _elm_lang$svg$Svg_Attributes$imageRendering = _elm_lang$virtual_dom$VirtualDom$attribute('image-rendering');
var _elm_lang$svg$Svg_Attributes$glyphOrientationVertical = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-vertical');
var _elm_lang$svg$Svg_Attributes$glyphOrientationHorizontal = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-horizontal');
var _elm_lang$svg$Svg_Attributes$fontWeight = _elm_lang$virtual_dom$VirtualDom$attribute('font-weight');
var _elm_lang$svg$Svg_Attributes$fontVariant = _elm_lang$virtual_dom$VirtualDom$attribute('font-variant');
var _elm_lang$svg$Svg_Attributes$fontStyle = _elm_lang$virtual_dom$VirtualDom$attribute('font-style');
var _elm_lang$svg$Svg_Attributes$fontStretch = _elm_lang$virtual_dom$VirtualDom$attribute('font-stretch');
var _elm_lang$svg$Svg_Attributes$fontSize = _elm_lang$virtual_dom$VirtualDom$attribute('font-size');
var _elm_lang$svg$Svg_Attributes$fontSizeAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('font-size-adjust');
var _elm_lang$svg$Svg_Attributes$fontFamily = _elm_lang$virtual_dom$VirtualDom$attribute('font-family');
var _elm_lang$svg$Svg_Attributes$floodOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('flood-opacity');
var _elm_lang$svg$Svg_Attributes$floodColor = _elm_lang$virtual_dom$VirtualDom$attribute('flood-color');
var _elm_lang$svg$Svg_Attributes$filter = _elm_lang$virtual_dom$VirtualDom$attribute('filter');
var _elm_lang$svg$Svg_Attributes$fill = _elm_lang$virtual_dom$VirtualDom$attribute('fill');
var _elm_lang$svg$Svg_Attributes$fillRule = _elm_lang$virtual_dom$VirtualDom$attribute('fill-rule');
var _elm_lang$svg$Svg_Attributes$fillOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('fill-opacity');
var _elm_lang$svg$Svg_Attributes$enableBackground = _elm_lang$virtual_dom$VirtualDom$attribute('enable-background');
var _elm_lang$svg$Svg_Attributes$dominantBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('dominant-baseline');
var _elm_lang$svg$Svg_Attributes$display = _elm_lang$virtual_dom$VirtualDom$attribute('display');
var _elm_lang$svg$Svg_Attributes$direction = _elm_lang$virtual_dom$VirtualDom$attribute('direction');
var _elm_lang$svg$Svg_Attributes$cursor = _elm_lang$virtual_dom$VirtualDom$attribute('cursor');
var _elm_lang$svg$Svg_Attributes$color = _elm_lang$virtual_dom$VirtualDom$attribute('color');
var _elm_lang$svg$Svg_Attributes$colorRendering = _elm_lang$virtual_dom$VirtualDom$attribute('color-rendering');
var _elm_lang$svg$Svg_Attributes$colorProfile = _elm_lang$virtual_dom$VirtualDom$attribute('color-profile');
var _elm_lang$svg$Svg_Attributes$colorInterpolation = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation');
var _elm_lang$svg$Svg_Attributes$colorInterpolationFilters = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation-filters');
var _elm_lang$svg$Svg_Attributes$clip = _elm_lang$virtual_dom$VirtualDom$attribute('clip');
var _elm_lang$svg$Svg_Attributes$clipRule = _elm_lang$virtual_dom$VirtualDom$attribute('clip-rule');
var _elm_lang$svg$Svg_Attributes$clipPath = _elm_lang$virtual_dom$VirtualDom$attribute('clip-path');
var _elm_lang$svg$Svg_Attributes$baselineShift = _elm_lang$virtual_dom$VirtualDom$attribute('baseline-shift');
var _elm_lang$svg$Svg_Attributes$alignmentBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('alignment-baseline');
var _elm_lang$svg$Svg_Attributes$zoomAndPan = _elm_lang$virtual_dom$VirtualDom$attribute('zoomAndPan');
var _elm_lang$svg$Svg_Attributes$z = _elm_lang$virtual_dom$VirtualDom$attribute('z');
var _elm_lang$svg$Svg_Attributes$yChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('yChannelSelector');
var _elm_lang$svg$Svg_Attributes$y2 = _elm_lang$virtual_dom$VirtualDom$attribute('y2');
var _elm_lang$svg$Svg_Attributes$y1 = _elm_lang$virtual_dom$VirtualDom$attribute('y1');
var _elm_lang$svg$Svg_Attributes$y = _elm_lang$virtual_dom$VirtualDom$attribute('y');
var _elm_lang$svg$Svg_Attributes$xmlSpace = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var _elm_lang$svg$Svg_Attributes$xmlLang = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:lang');
var _elm_lang$svg$Svg_Attributes$xmlBase = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:base');
var _elm_lang$svg$Svg_Attributes$xlinkType = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:type');
var _elm_lang$svg$Svg_Attributes$xlinkTitle = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:title');
var _elm_lang$svg$Svg_Attributes$xlinkShow = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:show');
var _elm_lang$svg$Svg_Attributes$xlinkRole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:role');
var _elm_lang$svg$Svg_Attributes$xlinkHref = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:href');
var _elm_lang$svg$Svg_Attributes$xlinkArcrole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:arcrole');
var _elm_lang$svg$Svg_Attributes$xlinkActuate = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:actuate');
var _elm_lang$svg$Svg_Attributes$xChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('xChannelSelector');
var _elm_lang$svg$Svg_Attributes$x2 = _elm_lang$virtual_dom$VirtualDom$attribute('x2');
var _elm_lang$svg$Svg_Attributes$x1 = _elm_lang$virtual_dom$VirtualDom$attribute('x1');
var _elm_lang$svg$Svg_Attributes$xHeight = _elm_lang$virtual_dom$VirtualDom$attribute('x-height');
var _elm_lang$svg$Svg_Attributes$x = _elm_lang$virtual_dom$VirtualDom$attribute('x');
var _elm_lang$svg$Svg_Attributes$widths = _elm_lang$virtual_dom$VirtualDom$attribute('widths');
var _elm_lang$svg$Svg_Attributes$width = _elm_lang$virtual_dom$VirtualDom$attribute('width');
var _elm_lang$svg$Svg_Attributes$viewTarget = _elm_lang$virtual_dom$VirtualDom$attribute('viewTarget');
var _elm_lang$svg$Svg_Attributes$viewBox = _elm_lang$virtual_dom$VirtualDom$attribute('viewBox');
var _elm_lang$svg$Svg_Attributes$vertOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-y');
var _elm_lang$svg$Svg_Attributes$vertOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-x');
var _elm_lang$svg$Svg_Attributes$vertAdvY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-adv-y');
var _elm_lang$svg$Svg_Attributes$version = _elm_lang$virtual_dom$VirtualDom$attribute('version');
var _elm_lang$svg$Svg_Attributes$values = _elm_lang$virtual_dom$VirtualDom$attribute('values');
var _elm_lang$svg$Svg_Attributes$vMathematical = _elm_lang$virtual_dom$VirtualDom$attribute('v-mathematical');
var _elm_lang$svg$Svg_Attributes$vIdeographic = _elm_lang$virtual_dom$VirtualDom$attribute('v-ideographic');
var _elm_lang$svg$Svg_Attributes$vHanging = _elm_lang$virtual_dom$VirtualDom$attribute('v-hanging');
var _elm_lang$svg$Svg_Attributes$vAlphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('v-alphabetic');
var _elm_lang$svg$Svg_Attributes$unitsPerEm = _elm_lang$virtual_dom$VirtualDom$attribute('units-per-em');
var _elm_lang$svg$Svg_Attributes$unicodeRange = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-range');
var _elm_lang$svg$Svg_Attributes$unicode = _elm_lang$virtual_dom$VirtualDom$attribute('unicode');
var _elm_lang$svg$Svg_Attributes$underlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('underline-thickness');
var _elm_lang$svg$Svg_Attributes$underlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('underline-position');
var _elm_lang$svg$Svg_Attributes$u2 = _elm_lang$virtual_dom$VirtualDom$attribute('u2');
var _elm_lang$svg$Svg_Attributes$u1 = _elm_lang$virtual_dom$VirtualDom$attribute('u1');
var _elm_lang$svg$Svg_Attributes$type_ = _elm_lang$virtual_dom$VirtualDom$attribute('type');
var _elm_lang$svg$Svg_Attributes$transform = _elm_lang$virtual_dom$VirtualDom$attribute('transform');
var _elm_lang$svg$Svg_Attributes$to = _elm_lang$virtual_dom$VirtualDom$attribute('to');
var _elm_lang$svg$Svg_Attributes$title = _elm_lang$virtual_dom$VirtualDom$attribute('title');
var _elm_lang$svg$Svg_Attributes$textLength = _elm_lang$virtual_dom$VirtualDom$attribute('textLength');
var _elm_lang$svg$Svg_Attributes$targetY = _elm_lang$virtual_dom$VirtualDom$attribute('targetY');
var _elm_lang$svg$Svg_Attributes$targetX = _elm_lang$virtual_dom$VirtualDom$attribute('targetX');
var _elm_lang$svg$Svg_Attributes$target = _elm_lang$virtual_dom$VirtualDom$attribute('target');
var _elm_lang$svg$Svg_Attributes$tableValues = _elm_lang$virtual_dom$VirtualDom$attribute('tableValues');
var _elm_lang$svg$Svg_Attributes$systemLanguage = _elm_lang$virtual_dom$VirtualDom$attribute('systemLanguage');
var _elm_lang$svg$Svg_Attributes$surfaceScale = _elm_lang$virtual_dom$VirtualDom$attribute('surfaceScale');
var _elm_lang$svg$Svg_Attributes$style = _elm_lang$virtual_dom$VirtualDom$attribute('style');
var _elm_lang$svg$Svg_Attributes$string = _elm_lang$virtual_dom$VirtualDom$attribute('string');
var _elm_lang$svg$Svg_Attributes$strikethroughThickness = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-thickness');
var _elm_lang$svg$Svg_Attributes$strikethroughPosition = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-position');
var _elm_lang$svg$Svg_Attributes$stitchTiles = _elm_lang$virtual_dom$VirtualDom$attribute('stitchTiles');
var _elm_lang$svg$Svg_Attributes$stemv = _elm_lang$virtual_dom$VirtualDom$attribute('stemv');
var _elm_lang$svg$Svg_Attributes$stemh = _elm_lang$virtual_dom$VirtualDom$attribute('stemh');
var _elm_lang$svg$Svg_Attributes$stdDeviation = _elm_lang$virtual_dom$VirtualDom$attribute('stdDeviation');
var _elm_lang$svg$Svg_Attributes$startOffset = _elm_lang$virtual_dom$VirtualDom$attribute('startOffset');
var _elm_lang$svg$Svg_Attributes$spreadMethod = _elm_lang$virtual_dom$VirtualDom$attribute('spreadMethod');
var _elm_lang$svg$Svg_Attributes$speed = _elm_lang$virtual_dom$VirtualDom$attribute('speed');
var _elm_lang$svg$Svg_Attributes$specularExponent = _elm_lang$virtual_dom$VirtualDom$attribute('specularExponent');
var _elm_lang$svg$Svg_Attributes$specularConstant = _elm_lang$virtual_dom$VirtualDom$attribute('specularConstant');
var _elm_lang$svg$Svg_Attributes$spacing = _elm_lang$virtual_dom$VirtualDom$attribute('spacing');
var _elm_lang$svg$Svg_Attributes$slope = _elm_lang$virtual_dom$VirtualDom$attribute('slope');
var _elm_lang$svg$Svg_Attributes$seed = _elm_lang$virtual_dom$VirtualDom$attribute('seed');
var _elm_lang$svg$Svg_Attributes$scale = _elm_lang$virtual_dom$VirtualDom$attribute('scale');
var _elm_lang$svg$Svg_Attributes$ry = _elm_lang$virtual_dom$VirtualDom$attribute('ry');
var _elm_lang$svg$Svg_Attributes$rx = _elm_lang$virtual_dom$VirtualDom$attribute('rx');
var _elm_lang$svg$Svg_Attributes$rotate = _elm_lang$virtual_dom$VirtualDom$attribute('rotate');
var _elm_lang$svg$Svg_Attributes$result = _elm_lang$virtual_dom$VirtualDom$attribute('result');
var _elm_lang$svg$Svg_Attributes$restart = _elm_lang$virtual_dom$VirtualDom$attribute('restart');
var _elm_lang$svg$Svg_Attributes$requiredFeatures = _elm_lang$virtual_dom$VirtualDom$attribute('requiredFeatures');
var _elm_lang$svg$Svg_Attributes$requiredExtensions = _elm_lang$virtual_dom$VirtualDom$attribute('requiredExtensions');
var _elm_lang$svg$Svg_Attributes$repeatDur = _elm_lang$virtual_dom$VirtualDom$attribute('repeatDur');
var _elm_lang$svg$Svg_Attributes$repeatCount = _elm_lang$virtual_dom$VirtualDom$attribute('repeatCount');
var _elm_lang$svg$Svg_Attributes$renderingIntent = _elm_lang$virtual_dom$VirtualDom$attribute('rendering-intent');
var _elm_lang$svg$Svg_Attributes$refY = _elm_lang$virtual_dom$VirtualDom$attribute('refY');
var _elm_lang$svg$Svg_Attributes$refX = _elm_lang$virtual_dom$VirtualDom$attribute('refX');
var _elm_lang$svg$Svg_Attributes$radius = _elm_lang$virtual_dom$VirtualDom$attribute('radius');
var _elm_lang$svg$Svg_Attributes$r = _elm_lang$virtual_dom$VirtualDom$attribute('r');
var _elm_lang$svg$Svg_Attributes$primitiveUnits = _elm_lang$virtual_dom$VirtualDom$attribute('primitiveUnits');
var _elm_lang$svg$Svg_Attributes$preserveAspectRatio = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAspectRatio');
var _elm_lang$svg$Svg_Attributes$preserveAlpha = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAlpha');
var _elm_lang$svg$Svg_Attributes$pointsAtZ = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtZ');
var _elm_lang$svg$Svg_Attributes$pointsAtY = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtY');
var _elm_lang$svg$Svg_Attributes$pointsAtX = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtX');
var _elm_lang$svg$Svg_Attributes$points = _elm_lang$virtual_dom$VirtualDom$attribute('points');
var _elm_lang$svg$Svg_Attributes$pointOrder = _elm_lang$virtual_dom$VirtualDom$attribute('point-order');
var _elm_lang$svg$Svg_Attributes$patternUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternUnits');
var _elm_lang$svg$Svg_Attributes$patternTransform = _elm_lang$virtual_dom$VirtualDom$attribute('patternTransform');
var _elm_lang$svg$Svg_Attributes$patternContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternContentUnits');
var _elm_lang$svg$Svg_Attributes$pathLength = _elm_lang$virtual_dom$VirtualDom$attribute('pathLength');
var _elm_lang$svg$Svg_Attributes$path = _elm_lang$virtual_dom$VirtualDom$attribute('path');
var _elm_lang$svg$Svg_Attributes$panose1 = _elm_lang$virtual_dom$VirtualDom$attribute('panose-1');
var _elm_lang$svg$Svg_Attributes$overlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('overline-thickness');
var _elm_lang$svg$Svg_Attributes$overlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('overline-position');
var _elm_lang$svg$Svg_Attributes$origin = _elm_lang$virtual_dom$VirtualDom$attribute('origin');
var _elm_lang$svg$Svg_Attributes$orientation = _elm_lang$virtual_dom$VirtualDom$attribute('orientation');
var _elm_lang$svg$Svg_Attributes$orient = _elm_lang$virtual_dom$VirtualDom$attribute('orient');
var _elm_lang$svg$Svg_Attributes$order = _elm_lang$virtual_dom$VirtualDom$attribute('order');
var _elm_lang$svg$Svg_Attributes$operator = _elm_lang$virtual_dom$VirtualDom$attribute('operator');
var _elm_lang$svg$Svg_Attributes$offset = _elm_lang$virtual_dom$VirtualDom$attribute('offset');
var _elm_lang$svg$Svg_Attributes$numOctaves = _elm_lang$virtual_dom$VirtualDom$attribute('numOctaves');
var _elm_lang$svg$Svg_Attributes$name = _elm_lang$virtual_dom$VirtualDom$attribute('name');
var _elm_lang$svg$Svg_Attributes$mode = _elm_lang$virtual_dom$VirtualDom$attribute('mode');
var _elm_lang$svg$Svg_Attributes$min = _elm_lang$virtual_dom$VirtualDom$attribute('min');
var _elm_lang$svg$Svg_Attributes$method = _elm_lang$virtual_dom$VirtualDom$attribute('method');
var _elm_lang$svg$Svg_Attributes$media = _elm_lang$virtual_dom$VirtualDom$attribute('media');
var _elm_lang$svg$Svg_Attributes$max = _elm_lang$virtual_dom$VirtualDom$attribute('max');
var _elm_lang$svg$Svg_Attributes$mathematical = _elm_lang$virtual_dom$VirtualDom$attribute('mathematical');
var _elm_lang$svg$Svg_Attributes$maskUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskUnits');
var _elm_lang$svg$Svg_Attributes$maskContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskContentUnits');
var _elm_lang$svg$Svg_Attributes$markerWidth = _elm_lang$virtual_dom$VirtualDom$attribute('markerWidth');
var _elm_lang$svg$Svg_Attributes$markerUnits = _elm_lang$virtual_dom$VirtualDom$attribute('markerUnits');
var _elm_lang$svg$Svg_Attributes$markerHeight = _elm_lang$virtual_dom$VirtualDom$attribute('markerHeight');
var _elm_lang$svg$Svg_Attributes$local = _elm_lang$virtual_dom$VirtualDom$attribute('local');
var _elm_lang$svg$Svg_Attributes$limitingConeAngle = _elm_lang$virtual_dom$VirtualDom$attribute('limitingConeAngle');
var _elm_lang$svg$Svg_Attributes$lengthAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('lengthAdjust');
var _elm_lang$svg$Svg_Attributes$lang = _elm_lang$virtual_dom$VirtualDom$attribute('lang');
var _elm_lang$svg$Svg_Attributes$keyTimes = _elm_lang$virtual_dom$VirtualDom$attribute('keyTimes');
var _elm_lang$svg$Svg_Attributes$keySplines = _elm_lang$virtual_dom$VirtualDom$attribute('keySplines');
var _elm_lang$svg$Svg_Attributes$keyPoints = _elm_lang$virtual_dom$VirtualDom$attribute('keyPoints');
var _elm_lang$svg$Svg_Attributes$kernelUnitLength = _elm_lang$virtual_dom$VirtualDom$attribute('kernelUnitLength');
var _elm_lang$svg$Svg_Attributes$kernelMatrix = _elm_lang$virtual_dom$VirtualDom$attribute('kernelMatrix');
var _elm_lang$svg$Svg_Attributes$k4 = _elm_lang$virtual_dom$VirtualDom$attribute('k4');
var _elm_lang$svg$Svg_Attributes$k3 = _elm_lang$virtual_dom$VirtualDom$attribute('k3');
var _elm_lang$svg$Svg_Attributes$k2 = _elm_lang$virtual_dom$VirtualDom$attribute('k2');
var _elm_lang$svg$Svg_Attributes$k1 = _elm_lang$virtual_dom$VirtualDom$attribute('k1');
var _elm_lang$svg$Svg_Attributes$k = _elm_lang$virtual_dom$VirtualDom$attribute('k');
var _elm_lang$svg$Svg_Attributes$intercept = _elm_lang$virtual_dom$VirtualDom$attribute('intercept');
var _elm_lang$svg$Svg_Attributes$in2 = _elm_lang$virtual_dom$VirtualDom$attribute('in2');
var _elm_lang$svg$Svg_Attributes$in_ = _elm_lang$virtual_dom$VirtualDom$attribute('in');
var _elm_lang$svg$Svg_Attributes$ideographic = _elm_lang$virtual_dom$VirtualDom$attribute('ideographic');
var _elm_lang$svg$Svg_Attributes$id = _elm_lang$virtual_dom$VirtualDom$attribute('id');
var _elm_lang$svg$Svg_Attributes$horizOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-y');
var _elm_lang$svg$Svg_Attributes$horizOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-x');
var _elm_lang$svg$Svg_Attributes$horizAdvX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-adv-x');
var _elm_lang$svg$Svg_Attributes$height = _elm_lang$virtual_dom$VirtualDom$attribute('height');
var _elm_lang$svg$Svg_Attributes$hanging = _elm_lang$virtual_dom$VirtualDom$attribute('hanging');
var _elm_lang$svg$Svg_Attributes$gradientUnits = _elm_lang$virtual_dom$VirtualDom$attribute('gradientUnits');
var _elm_lang$svg$Svg_Attributes$gradientTransform = _elm_lang$virtual_dom$VirtualDom$attribute('gradientTransform');
var _elm_lang$svg$Svg_Attributes$glyphRef = _elm_lang$virtual_dom$VirtualDom$attribute('glyphRef');
var _elm_lang$svg$Svg_Attributes$glyphName = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-name');
var _elm_lang$svg$Svg_Attributes$g2 = _elm_lang$virtual_dom$VirtualDom$attribute('g2');
var _elm_lang$svg$Svg_Attributes$g1 = _elm_lang$virtual_dom$VirtualDom$attribute('g1');
var _elm_lang$svg$Svg_Attributes$fy = _elm_lang$virtual_dom$VirtualDom$attribute('fy');
var _elm_lang$svg$Svg_Attributes$fx = _elm_lang$virtual_dom$VirtualDom$attribute('fx');
var _elm_lang$svg$Svg_Attributes$from = _elm_lang$virtual_dom$VirtualDom$attribute('from');
var _elm_lang$svg$Svg_Attributes$format = _elm_lang$virtual_dom$VirtualDom$attribute('format');
var _elm_lang$svg$Svg_Attributes$filterUnits = _elm_lang$virtual_dom$VirtualDom$attribute('filterUnits');
var _elm_lang$svg$Svg_Attributes$filterRes = _elm_lang$virtual_dom$VirtualDom$attribute('filterRes');
var _elm_lang$svg$Svg_Attributes$externalResourcesRequired = _elm_lang$virtual_dom$VirtualDom$attribute('externalResourcesRequired');
var _elm_lang$svg$Svg_Attributes$exponent = _elm_lang$virtual_dom$VirtualDom$attribute('exponent');
var _elm_lang$svg$Svg_Attributes$end = _elm_lang$virtual_dom$VirtualDom$attribute('end');
var _elm_lang$svg$Svg_Attributes$elevation = _elm_lang$virtual_dom$VirtualDom$attribute('elevation');
var _elm_lang$svg$Svg_Attributes$edgeMode = _elm_lang$virtual_dom$VirtualDom$attribute('edgeMode');
var _elm_lang$svg$Svg_Attributes$dy = _elm_lang$virtual_dom$VirtualDom$attribute('dy');
var _elm_lang$svg$Svg_Attributes$dx = _elm_lang$virtual_dom$VirtualDom$attribute('dx');
var _elm_lang$svg$Svg_Attributes$dur = _elm_lang$virtual_dom$VirtualDom$attribute('dur');
var _elm_lang$svg$Svg_Attributes$divisor = _elm_lang$virtual_dom$VirtualDom$attribute('divisor');
var _elm_lang$svg$Svg_Attributes$diffuseConstant = _elm_lang$virtual_dom$VirtualDom$attribute('diffuseConstant');
var _elm_lang$svg$Svg_Attributes$descent = _elm_lang$virtual_dom$VirtualDom$attribute('descent');
var _elm_lang$svg$Svg_Attributes$decelerate = _elm_lang$virtual_dom$VirtualDom$attribute('decelerate');
var _elm_lang$svg$Svg_Attributes$d = _elm_lang$virtual_dom$VirtualDom$attribute('d');
var _elm_lang$svg$Svg_Attributes$cy = _elm_lang$virtual_dom$VirtualDom$attribute('cy');
var _elm_lang$svg$Svg_Attributes$cx = _elm_lang$virtual_dom$VirtualDom$attribute('cx');
var _elm_lang$svg$Svg_Attributes$contentStyleType = _elm_lang$virtual_dom$VirtualDom$attribute('contentStyleType');
var _elm_lang$svg$Svg_Attributes$contentScriptType = _elm_lang$virtual_dom$VirtualDom$attribute('contentScriptType');
var _elm_lang$svg$Svg_Attributes$clipPathUnits = _elm_lang$virtual_dom$VirtualDom$attribute('clipPathUnits');
var _elm_lang$svg$Svg_Attributes$class = _elm_lang$virtual_dom$VirtualDom$attribute('class');
var _elm_lang$svg$Svg_Attributes$capHeight = _elm_lang$virtual_dom$VirtualDom$attribute('cap-height');
var _elm_lang$svg$Svg_Attributes$calcMode = _elm_lang$virtual_dom$VirtualDom$attribute('calcMode');
var _elm_lang$svg$Svg_Attributes$by = _elm_lang$virtual_dom$VirtualDom$attribute('by');
var _elm_lang$svg$Svg_Attributes$bias = _elm_lang$virtual_dom$VirtualDom$attribute('bias');
var _elm_lang$svg$Svg_Attributes$begin = _elm_lang$virtual_dom$VirtualDom$attribute('begin');
var _elm_lang$svg$Svg_Attributes$bbox = _elm_lang$virtual_dom$VirtualDom$attribute('bbox');
var _elm_lang$svg$Svg_Attributes$baseProfile = _elm_lang$virtual_dom$VirtualDom$attribute('baseProfile');
var _elm_lang$svg$Svg_Attributes$baseFrequency = _elm_lang$virtual_dom$VirtualDom$attribute('baseFrequency');
var _elm_lang$svg$Svg_Attributes$azimuth = _elm_lang$virtual_dom$VirtualDom$attribute('azimuth');
var _elm_lang$svg$Svg_Attributes$autoReverse = _elm_lang$virtual_dom$VirtualDom$attribute('autoReverse');
var _elm_lang$svg$Svg_Attributes$attributeType = _elm_lang$virtual_dom$VirtualDom$attribute('attributeType');
var _elm_lang$svg$Svg_Attributes$attributeName = _elm_lang$virtual_dom$VirtualDom$attribute('attributeName');
var _elm_lang$svg$Svg_Attributes$ascent = _elm_lang$virtual_dom$VirtualDom$attribute('ascent');
var _elm_lang$svg$Svg_Attributes$arabicForm = _elm_lang$virtual_dom$VirtualDom$attribute('arabic-form');
var _elm_lang$svg$Svg_Attributes$amplitude = _elm_lang$virtual_dom$VirtualDom$attribute('amplitude');
var _elm_lang$svg$Svg_Attributes$allowReorder = _elm_lang$virtual_dom$VirtualDom$attribute('allowReorder');
var _elm_lang$svg$Svg_Attributes$alphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('alphabetic');
var _elm_lang$svg$Svg_Attributes$additive = _elm_lang$virtual_dom$VirtualDom$attribute('additive');
var _elm_lang$svg$Svg_Attributes$accumulate = _elm_lang$virtual_dom$VirtualDom$attribute('accumulate');
var _elm_lang$svg$Svg_Attributes$accelerate = _elm_lang$virtual_dom$VirtualDom$attribute('accelerate');
var _elm_lang$svg$Svg_Attributes$accentHeight = _elm_lang$virtual_dom$VirtualDom$attribute('accent-height');

//import Result //

var _elm_lang$core$Native_Date = function() {

function fromString(str)
{
	var date = new Date(str);
	return isNaN(date.getTime())
		? _elm_lang$core$Result$Err('Unable to parse \'' + str + '\' as a date. Dates must be in the ISO 8601 format.')
		: _elm_lang$core$Result$Ok(date);
}

var dayTable = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
var monthTable =
	['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
	 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];


return {
	fromString: fromString,
	year: function(d) { return d.getFullYear(); },
	month: function(d) { return { ctor: monthTable[d.getMonth()] }; },
	day: function(d) { return d.getDate(); },
	hour: function(d) { return d.getHours(); },
	minute: function(d) { return d.getMinutes(); },
	second: function(d) { return d.getSeconds(); },
	millisecond: function(d) { return d.getMilliseconds(); },
	toTime: function(d) { return d.getTime(); },
	fromTime: function(t) { return new Date(t); },
	dayOfWeek: function(d) { return { ctor: dayTable[d.getDay()] }; }
};

}();
//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _elm_lang$core$Date$millisecond = _elm_lang$core$Native_Date.millisecond;
var _elm_lang$core$Date$second = _elm_lang$core$Native_Date.second;
var _elm_lang$core$Date$minute = _elm_lang$core$Native_Date.minute;
var _elm_lang$core$Date$hour = _elm_lang$core$Native_Date.hour;
var _elm_lang$core$Date$dayOfWeek = _elm_lang$core$Native_Date.dayOfWeek;
var _elm_lang$core$Date$day = _elm_lang$core$Native_Date.day;
var _elm_lang$core$Date$month = _elm_lang$core$Native_Date.month;
var _elm_lang$core$Date$year = _elm_lang$core$Native_Date.year;
var _elm_lang$core$Date$fromTime = _elm_lang$core$Native_Date.fromTime;
var _elm_lang$core$Date$toTime = _elm_lang$core$Native_Date.toTime;
var _elm_lang$core$Date$fromString = _elm_lang$core$Native_Date.fromString;
var _elm_lang$core$Date$now = A2(_elm_lang$core$Task$map, _elm_lang$core$Date$fromTime, _elm_lang$core$Time$now);
var _elm_lang$core$Date$Date = {ctor: 'Date'};
var _elm_lang$core$Date$Sun = {ctor: 'Sun'};
var _elm_lang$core$Date$Sat = {ctor: 'Sat'};
var _elm_lang$core$Date$Fri = {ctor: 'Fri'};
var _elm_lang$core$Date$Thu = {ctor: 'Thu'};
var _elm_lang$core$Date$Wed = {ctor: 'Wed'};
var _elm_lang$core$Date$Tue = {ctor: 'Tue'};
var _elm_lang$core$Date$Mon = {ctor: 'Mon'};
var _elm_lang$core$Date$Dec = {ctor: 'Dec'};
var _elm_lang$core$Date$Nov = {ctor: 'Nov'};
var _elm_lang$core$Date$Oct = {ctor: 'Oct'};
var _elm_lang$core$Date$Sep = {ctor: 'Sep'};
var _elm_lang$core$Date$Aug = {ctor: 'Aug'};
var _elm_lang$core$Date$Jul = {ctor: 'Jul'};
var _elm_lang$core$Date$Jun = {ctor: 'Jun'};
var _elm_lang$core$Date$May = {ctor: 'May'};
var _elm_lang$core$Date$Apr = {ctor: 'Apr'};
var _elm_lang$core$Date$Mar = {ctor: 'Mar'};
var _elm_lang$core$Date$Feb = {ctor: 'Feb'};
var _elm_lang$core$Date$Jan = {ctor: 'Jan'};

var _rluiten$elm_date_extra$Date_Extra_Core$prevMonth = function (month) {
	var _p0 = month;
	switch (_p0.ctor) {
		case 'Jan':
			return _elm_lang$core$Date$Dec;
		case 'Feb':
			return _elm_lang$core$Date$Jan;
		case 'Mar':
			return _elm_lang$core$Date$Feb;
		case 'Apr':
			return _elm_lang$core$Date$Mar;
		case 'May':
			return _elm_lang$core$Date$Apr;
		case 'Jun':
			return _elm_lang$core$Date$May;
		case 'Jul':
			return _elm_lang$core$Date$Jun;
		case 'Aug':
			return _elm_lang$core$Date$Jul;
		case 'Sep':
			return _elm_lang$core$Date$Aug;
		case 'Oct':
			return _elm_lang$core$Date$Sep;
		case 'Nov':
			return _elm_lang$core$Date$Oct;
		default:
			return _elm_lang$core$Date$Nov;
	}
};
var _rluiten$elm_date_extra$Date_Extra_Core$nextMonth = function (month) {
	var _p1 = month;
	switch (_p1.ctor) {
		case 'Jan':
			return _elm_lang$core$Date$Feb;
		case 'Feb':
			return _elm_lang$core$Date$Mar;
		case 'Mar':
			return _elm_lang$core$Date$Apr;
		case 'Apr':
			return _elm_lang$core$Date$May;
		case 'May':
			return _elm_lang$core$Date$Jun;
		case 'Jun':
			return _elm_lang$core$Date$Jul;
		case 'Jul':
			return _elm_lang$core$Date$Aug;
		case 'Aug':
			return _elm_lang$core$Date$Sep;
		case 'Sep':
			return _elm_lang$core$Date$Oct;
		case 'Oct':
			return _elm_lang$core$Date$Nov;
		case 'Nov':
			return _elm_lang$core$Date$Dec;
		default:
			return _elm_lang$core$Date$Jan;
	}
};
var _rluiten$elm_date_extra$Date_Extra_Core$intToMonth = function (month) {
	return (_elm_lang$core$Native_Utils.cmp(month, 1) < 1) ? _elm_lang$core$Date$Jan : (_elm_lang$core$Native_Utils.eq(month, 2) ? _elm_lang$core$Date$Feb : (_elm_lang$core$Native_Utils.eq(month, 3) ? _elm_lang$core$Date$Mar : (_elm_lang$core$Native_Utils.eq(month, 4) ? _elm_lang$core$Date$Apr : (_elm_lang$core$Native_Utils.eq(month, 5) ? _elm_lang$core$Date$May : (_elm_lang$core$Native_Utils.eq(month, 6) ? _elm_lang$core$Date$Jun : (_elm_lang$core$Native_Utils.eq(month, 7) ? _elm_lang$core$Date$Jul : (_elm_lang$core$Native_Utils.eq(month, 8) ? _elm_lang$core$Date$Aug : (_elm_lang$core$Native_Utils.eq(month, 9) ? _elm_lang$core$Date$Sep : (_elm_lang$core$Native_Utils.eq(month, 10) ? _elm_lang$core$Date$Oct : (_elm_lang$core$Native_Utils.eq(month, 11) ? _elm_lang$core$Date$Nov : _elm_lang$core$Date$Dec))))))))));
};
var _rluiten$elm_date_extra$Date_Extra_Core$monthToInt = function (month) {
	var _p2 = month;
	switch (_p2.ctor) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var _rluiten$elm_date_extra$Date_Extra_Core$isLeapYear = function (year) {
	return (_elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], year, 4),
		0) && (!_elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], year, 100),
		0))) || _elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], year, 400),
		0);
};
var _rluiten$elm_date_extra$Date_Extra_Core$isLeapYearDate = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Core$isLeapYear(
		_elm_lang$core$Date$year(date));
};
var _rluiten$elm_date_extra$Date_Extra_Core$yearToDayLength = function (year) {
	return _rluiten$elm_date_extra$Date_Extra_Core$isLeapYear(year) ? 366 : 365;
};
var _rluiten$elm_date_extra$Date_Extra_Core$daysInMonth = F2(
	function (year, month) {
		var _p3 = month;
		switch (_p3.ctor) {
			case 'Jan':
				return 31;
			case 'Feb':
				return _rluiten$elm_date_extra$Date_Extra_Core$isLeapYear(year) ? 29 : 28;
			case 'Mar':
				return 31;
			case 'Apr':
				return 30;
			case 'May':
				return 31;
			case 'Jun':
				return 30;
			case 'Jul':
				return 31;
			case 'Aug':
				return 31;
			case 'Sep':
				return 30;
			case 'Oct':
				return 31;
			case 'Nov':
				return 30;
			default:
				return 31;
		}
	});
var _rluiten$elm_date_extra$Date_Extra_Core$daysInMonthDate = function (date) {
	return A2(
		_rluiten$elm_date_extra$Date_Extra_Core$daysInMonth,
		_elm_lang$core$Date$year(date),
		_elm_lang$core$Date$month(date));
};
var _rluiten$elm_date_extra$Date_Extra_Core$monthList = {
	ctor: '::',
	_0: _elm_lang$core$Date$Jan,
	_1: {
		ctor: '::',
		_0: _elm_lang$core$Date$Feb,
		_1: {
			ctor: '::',
			_0: _elm_lang$core$Date$Mar,
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Date$Apr,
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Date$May,
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Date$Jun,
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Date$Jul,
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Date$Aug,
								_1: {
									ctor: '::',
									_0: _elm_lang$core$Date$Sep,
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Date$Oct,
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Date$Nov,
											_1: {
												ctor: '::',
												_0: _elm_lang$core$Date$Dec,
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _rluiten$elm_date_extra$Date_Extra_Core$toTime = function (_p4) {
	return _elm_lang$core$Basics$floor(
		_elm_lang$core$Date$toTime(_p4));
};
var _rluiten$elm_date_extra$Date_Extra_Core$fromTime = function (_p5) {
	return _elm_lang$core$Date$fromTime(
		_elm_lang$core$Basics$toFloat(_p5));
};
var _rluiten$elm_date_extra$Date_Extra_Core$prevDay = function (day) {
	var _p6 = day;
	switch (_p6.ctor) {
		case 'Mon':
			return _elm_lang$core$Date$Sun;
		case 'Tue':
			return _elm_lang$core$Date$Mon;
		case 'Wed':
			return _elm_lang$core$Date$Tue;
		case 'Thu':
			return _elm_lang$core$Date$Wed;
		case 'Fri':
			return _elm_lang$core$Date$Thu;
		case 'Sat':
			return _elm_lang$core$Date$Fri;
		default:
			return _elm_lang$core$Date$Sat;
	}
};
var _rluiten$elm_date_extra$Date_Extra_Core$nextDay = function (day) {
	var _p7 = day;
	switch (_p7.ctor) {
		case 'Mon':
			return _elm_lang$core$Date$Tue;
		case 'Tue':
			return _elm_lang$core$Date$Wed;
		case 'Wed':
			return _elm_lang$core$Date$Thu;
		case 'Thu':
			return _elm_lang$core$Date$Fri;
		case 'Fri':
			return _elm_lang$core$Date$Sat;
		case 'Sat':
			return _elm_lang$core$Date$Sun;
		default:
			return _elm_lang$core$Date$Mon;
	}
};
var _rluiten$elm_date_extra$Date_Extra_Core$isoDayOfWeek = function (day) {
	var _p8 = day;
	switch (_p8.ctor) {
		case 'Mon':
			return 1;
		case 'Tue':
			return 2;
		case 'Wed':
			return 3;
		case 'Thu':
			return 4;
		case 'Fri':
			return 5;
		case 'Sat':
			return 6;
		default:
			return 7;
	}
};
var _rluiten$elm_date_extra$Date_Extra_Core$daysBackToStartOfWeek = F2(
	function (dateDay, startOfWeekDay) {
		var startOfWeekDayIndex = _rluiten$elm_date_extra$Date_Extra_Core$isoDayOfWeek(startOfWeekDay);
		var dateDayIndex = _rluiten$elm_date_extra$Date_Extra_Core$isoDayOfWeek(dateDay);
		return (_elm_lang$core$Native_Utils.cmp(dateDayIndex, startOfWeekDayIndex) < 0) ? ((7 + dateDayIndex) - startOfWeekDayIndex) : (dateDayIndex - startOfWeekDayIndex);
	});
var _rluiten$elm_date_extra$Date_Extra_Core$ticksAMillisecond = _elm_lang$core$Basics$floor(_elm_lang$core$Time$millisecond);
var _rluiten$elm_date_extra$Date_Extra_Core$ticksASecond = _rluiten$elm_date_extra$Date_Extra_Core$ticksAMillisecond * 1000;
var _rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute = _rluiten$elm_date_extra$Date_Extra_Core$ticksASecond * 60;
var _rluiten$elm_date_extra$Date_Extra_Core$ticksAnHour = _rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute * 60;
var _rluiten$elm_date_extra$Date_Extra_Core$ticksADay = _rluiten$elm_date_extra$Date_Extra_Core$ticksAnHour * 24;
var _rluiten$elm_date_extra$Date_Extra_Core$ticksAWeek = _rluiten$elm_date_extra$Date_Extra_Core$ticksADay * 7;
var _rluiten$elm_date_extra$Date_Extra_Core$firstOfMonthTicks = function (date) {
	var dateTicks = _rluiten$elm_date_extra$Date_Extra_Core$toTime(date);
	var day = _elm_lang$core$Date$day(date);
	return dateTicks + ((1 - day) * _rluiten$elm_date_extra$Date_Extra_Core$ticksADay);
};
var _rluiten$elm_date_extra$Date_Extra_Core$lastOfPrevMonthDate = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Core$fromTime(
		_rluiten$elm_date_extra$Date_Extra_Core$firstOfMonthTicks(date) - _rluiten$elm_date_extra$Date_Extra_Core$ticksADay);
};
var _rluiten$elm_date_extra$Date_Extra_Core$daysInPrevMonth = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Core$daysInMonthDate(
		_rluiten$elm_date_extra$Date_Extra_Core$lastOfPrevMonthDate(date));
};
var _rluiten$elm_date_extra$Date_Extra_Core$toFirstOfMonth = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Core$fromTime(
		_rluiten$elm_date_extra$Date_Extra_Core$firstOfMonthTicks(date));
};
var _rluiten$elm_date_extra$Date_Extra_Core$lastOfMonthTicks = function (date) {
	var dateTicks = _rluiten$elm_date_extra$Date_Extra_Core$toTime(date);
	var day = _elm_lang$core$Date$day(date);
	var month = _elm_lang$core$Date$month(date);
	var year = _elm_lang$core$Date$year(date);
	var daysInMonthVal = A2(_rluiten$elm_date_extra$Date_Extra_Core$daysInMonth, year, month);
	var addDays = daysInMonthVal - day;
	return dateTicks + (addDays * _rluiten$elm_date_extra$Date_Extra_Core$ticksADay);
};
var _rluiten$elm_date_extra$Date_Extra_Core$firstOfNextMonthDate = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Core$fromTime(
		_rluiten$elm_date_extra$Date_Extra_Core$lastOfMonthTicks(date) + _rluiten$elm_date_extra$Date_Extra_Core$ticksADay);
};
var _rluiten$elm_date_extra$Date_Extra_Core$daysInNextMonth = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Core$daysInMonthDate(
		_rluiten$elm_date_extra$Date_Extra_Core$firstOfNextMonthDate(date));
};
var _rluiten$elm_date_extra$Date_Extra_Core$lastOfMonthDate = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Core$fromTime(
		_rluiten$elm_date_extra$Date_Extra_Core$lastOfMonthTicks(date));
};
var _rluiten$elm_date_extra$Date_Extra_Core$epochDateStr = '1970-01-01T00:00:00Z';

//import Maybe, Native.List //

var _elm_lang$core$Native_Regex = function() {

function escape(str)
{
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}
function caseInsensitive(re)
{
	return new RegExp(re.source, 'gi');
}
function regex(raw)
{
	return new RegExp(raw, 'g');
}

function contains(re, string)
{
	return string.match(re) !== null;
}

function find(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex === re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		out.push({
			match: result[0],
			submatches: _elm_lang$core$Native_List.fromArray(subs),
			index: result.index,
			number: number
		});
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

function replace(n, re, replacer, string)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		return replacer({
			match: match,
			submatches: _elm_lang$core$Native_List.fromArray(submatches),
			index: arguments[arguments.length - 2],
			number: count
		});
	}
	return string.replace(re, jsReplacer);
}

function split(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	if (n === Infinity)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(re));
	}
	var string = str;
	var result;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		if (!(result = re.exec(string))) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

return {
	regex: regex,
	caseInsensitive: caseInsensitive,
	escape: escape,

	contains: F2(contains),
	find: F3(find),
	replace: F4(replace),
	split: F3(split)
};

}();

var _elm_lang$core$Regex$split = _elm_lang$core$Native_Regex.split;
var _elm_lang$core$Regex$replace = _elm_lang$core$Native_Regex.replace;
var _elm_lang$core$Regex$find = _elm_lang$core$Native_Regex.find;
var _elm_lang$core$Regex$contains = _elm_lang$core$Native_Regex.contains;
var _elm_lang$core$Regex$caseInsensitive = _elm_lang$core$Native_Regex.caseInsensitive;
var _elm_lang$core$Regex$regex = _elm_lang$core$Native_Regex.regex;
var _elm_lang$core$Regex$escape = _elm_lang$core$Native_Regex.escape;
var _elm_lang$core$Regex$Match = F4(
	function (a, b, c, d) {
		return {match: a, submatches: b, index: c, number: d};
	});
var _elm_lang$core$Regex$Regex = {ctor: 'Regex'};
var _elm_lang$core$Regex$AtMost = function (a) {
	return {ctor: 'AtMost', _0: a};
};
var _elm_lang$core$Regex$All = {ctor: 'All'};

var _rluiten$elm_date_extra$Date_Extra_Config$Config = F2(
	function (a, b) {
		return {i18n: a, format: b};
	});

var _rluiten$elm_date_extra$Date_Extra_Period$diff = F2(
	function (date1, date2) {
		var millisecondDiff = _elm_lang$core$Date$millisecond(date1) - _elm_lang$core$Date$millisecond(date2);
		var secondDiff = _elm_lang$core$Date$second(date1) - _elm_lang$core$Date$second(date2);
		var minuteDiff = _elm_lang$core$Date$minute(date1) - _elm_lang$core$Date$minute(date2);
		var hourDiff = _elm_lang$core$Date$hour(date1) - _elm_lang$core$Date$hour(date2);
		var ticksDiff = _rluiten$elm_date_extra$Date_Extra_Core$toTime(date1) - _rluiten$elm_date_extra$Date_Extra_Core$toTime(date2);
		var ticksDayDiff = (((ticksDiff - (hourDiff * _rluiten$elm_date_extra$Date_Extra_Core$ticksAnHour)) - (minuteDiff * _rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute)) - (secondDiff * _rluiten$elm_date_extra$Date_Extra_Core$ticksASecond)) - (millisecondDiff * _rluiten$elm_date_extra$Date_Extra_Core$ticksAMillisecond);
		var onlyDaysDiff = (ticksDayDiff / _rluiten$elm_date_extra$Date_Extra_Core$ticksADay) | 0;
		var _p0 = function () {
			if (_elm_lang$core$Native_Utils.cmp(onlyDaysDiff, 0) < 0) {
				var absDayDiff = _elm_lang$core$Basics$abs(onlyDaysDiff);
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Basics$negate((absDayDiff / 7) | 0),
					_1: _elm_lang$core$Basics$negate(
						A2(_elm_lang$core$Basics_ops['%'], absDayDiff, 7))
				};
			} else {
				return {
					ctor: '_Tuple2',
					_0: (onlyDaysDiff / 7) | 0,
					_1: A2(_elm_lang$core$Basics_ops['%'], onlyDaysDiff, 7)
				};
			}
		}();
		var weekDiff = _p0._0;
		var dayDiff = _p0._1;
		return {week: weekDiff, day: dayDiff, hour: hourDiff, minute: minuteDiff, second: secondDiff, millisecond: millisecondDiff};
	});
var _rluiten$elm_date_extra$Date_Extra_Period$addTimeUnit = F3(
	function (unit, addend, date) {
		return _rluiten$elm_date_extra$Date_Extra_Core$fromTime(
			A2(
				F2(
					function (x, y) {
						return x + y;
					}),
				addend * unit,
				_rluiten$elm_date_extra$Date_Extra_Core$toTime(date)));
	});
var _rluiten$elm_date_extra$Date_Extra_Period$toTicks = function (period) {
	var _p1 = period;
	switch (_p1.ctor) {
		case 'Millisecond':
			return _rluiten$elm_date_extra$Date_Extra_Core$ticksAMillisecond;
		case 'Second':
			return _rluiten$elm_date_extra$Date_Extra_Core$ticksASecond;
		case 'Minute':
			return _rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute;
		case 'Hour':
			return _rluiten$elm_date_extra$Date_Extra_Core$ticksAnHour;
		case 'Day':
			return _rluiten$elm_date_extra$Date_Extra_Core$ticksADay;
		case 'Week':
			return _rluiten$elm_date_extra$Date_Extra_Core$ticksAWeek;
		default:
			var _p2 = _p1._0;
			return (((((_rluiten$elm_date_extra$Date_Extra_Core$ticksAMillisecond * _p2.millisecond) + (_rluiten$elm_date_extra$Date_Extra_Core$ticksASecond * _p2.second)) + (_rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute * _p2.minute)) + (_rluiten$elm_date_extra$Date_Extra_Core$ticksAnHour * _p2.hour)) + (_rluiten$elm_date_extra$Date_Extra_Core$ticksADay * _p2.day)) + (_rluiten$elm_date_extra$Date_Extra_Core$ticksAWeek * _p2.week);
	}
};
var _rluiten$elm_date_extra$Date_Extra_Period$add = function (period) {
	return _rluiten$elm_date_extra$Date_Extra_Period$addTimeUnit(
		_rluiten$elm_date_extra$Date_Extra_Period$toTicks(period));
};
var _rluiten$elm_date_extra$Date_Extra_Period$zeroDelta = {week: 0, day: 0, hour: 0, minute: 0, second: 0, millisecond: 0};
var _rluiten$elm_date_extra$Date_Extra_Period$DeltaRecord = F6(
	function (a, b, c, d, e, f) {
		return {week: a, day: b, hour: c, minute: d, second: e, millisecond: f};
	});
var _rluiten$elm_date_extra$Date_Extra_Period$Delta = function (a) {
	return {ctor: 'Delta', _0: a};
};
var _rluiten$elm_date_extra$Date_Extra_Period$Week = {ctor: 'Week'};
var _rluiten$elm_date_extra$Date_Extra_Period$Day = {ctor: 'Day'};
var _rluiten$elm_date_extra$Date_Extra_Period$Hour = {ctor: 'Hour'};
var _rluiten$elm_date_extra$Date_Extra_Period$Minute = {ctor: 'Minute'};
var _rluiten$elm_date_extra$Date_Extra_Period$Second = {ctor: 'Second'};
var _rluiten$elm_date_extra$Date_Extra_Period$Millisecond = {ctor: 'Millisecond'};

var _rluiten$elm_date_extra$Date_Extra_Internal$daysFromCivil = F3(
	function (year, month, day) {
		var doy = (((((153 * (month + ((_elm_lang$core$Native_Utils.cmp(month, 2) > 0) ? -3 : 9))) + 2) / 5) | 0) + day) - 1;
		var y = year - ((_elm_lang$core$Native_Utils.cmp(month, 2) < 1) ? 1 : 0);
		var era = (((_elm_lang$core$Native_Utils.cmp(y, 0) > -1) ? y : (y - 399)) / 400) | 0;
		var yoe = y - (era * 400);
		var doe = (((yoe * 365) + ((yoe / 4) | 0)) - ((yoe / 100) | 0)) + doy;
		return ((era * 146097) + doe) - 719468;
	});
var _rluiten$elm_date_extra$Date_Extra_Internal$ticksFromFields = F7(
	function (year, month, day, hour, minute, second, millisecond) {
		var monthInt = _rluiten$elm_date_extra$Date_Extra_Core$monthToInt(month);
		var c_year = (_elm_lang$core$Native_Utils.cmp(year, 0) < 0) ? 0 : year;
		var c_day = A3(
			_elm_lang$core$Basics$clamp,
			1,
			A2(_rluiten$elm_date_extra$Date_Extra_Core$daysInMonth, c_year, month),
			day);
		var dayCount = A3(_rluiten$elm_date_extra$Date_Extra_Internal$daysFromCivil, c_year, monthInt, c_day);
		return _rluiten$elm_date_extra$Date_Extra_Period$toTicks(
			_rluiten$elm_date_extra$Date_Extra_Period$Delta(
				{
					millisecond: A3(_elm_lang$core$Basics$clamp, 0, 999, millisecond),
					second: A3(_elm_lang$core$Basics$clamp, 0, 59, second),
					minute: A3(_elm_lang$core$Basics$clamp, 0, 59, minute),
					hour: A3(_elm_lang$core$Basics$clamp, 0, 23, hour),
					day: dayCount,
					week: 0
				}));
	});
var _rluiten$elm_date_extra$Date_Extra_Internal$ticksFromDateFields = function (date) {
	return A7(
		_rluiten$elm_date_extra$Date_Extra_Internal$ticksFromFields,
		_elm_lang$core$Date$year(date),
		_elm_lang$core$Date$month(date),
		_elm_lang$core$Date$day(date),
		_elm_lang$core$Date$hour(date),
		_elm_lang$core$Date$minute(date),
		_elm_lang$core$Date$second(date),
		_elm_lang$core$Date$millisecond(date));
};
var _rluiten$elm_date_extra$Date_Extra_Internal$getTimezoneOffset = function (date) {
	var v1Ticks = _rluiten$elm_date_extra$Date_Extra_Internal$ticksFromDateFields(date);
	var dateTicks = _elm_lang$core$Basics$floor(
		_elm_lang$core$Date$toTime(date));
	return ((dateTicks - v1Ticks) / _rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute) | 0;
};
var _rluiten$elm_date_extra$Date_Extra_Internal$hackDateAsOffset = F2(
	function (offsetMinutes, date) {
		return _rluiten$elm_date_extra$Date_Extra_Core$fromTime(
			A2(
				F2(
					function (x, y) {
						return x + y;
					}),
				offsetMinutes * _rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute,
				_rluiten$elm_date_extra$Date_Extra_Core$toTime(date)));
	});
var _rluiten$elm_date_extra$Date_Extra_Internal$hackDateAsUtc = function (date) {
	var offset = _rluiten$elm_date_extra$Date_Extra_Internal$getTimezoneOffset(date);
	var oHours = (offset / _rluiten$elm_date_extra$Date_Extra_Core$ticksAnHour) | 0;
	var oMinutes = ((offset - (oHours * _rluiten$elm_date_extra$Date_Extra_Core$ticksAnHour)) / _rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute) | 0;
	return A2(_rluiten$elm_date_extra$Date_Extra_Internal$hackDateAsOffset, offset, date);
};

var _rluiten$elm_date_extra$Date_Extra_Create$epochDate = _elm_lang$core$Date$fromTime(0);
var _rluiten$elm_date_extra$Date_Extra_Create$epochTimezoneOffset = function () {
	var inMinutes = (_elm_lang$core$Date$hour(_rluiten$elm_date_extra$Date_Extra_Create$epochDate) * 60) + _elm_lang$core$Date$minute(_rluiten$elm_date_extra$Date_Extra_Create$epochDate);
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Date$year(_rluiten$elm_date_extra$Date_Extra_Create$epochDate),
		1969) ? (0 - (inMinutes - (24 * 60))) : (0 - inMinutes);
}();
var _rluiten$elm_date_extra$Date_Extra_Create$getTimezoneOffset = _rluiten$elm_date_extra$Date_Extra_Internal$getTimezoneOffset;
var _rluiten$elm_date_extra$Date_Extra_Create$adjustedTicksToDate = function (ticks) {
	var date = A3(_rluiten$elm_date_extra$Date_Extra_Period$add, _rluiten$elm_date_extra$Date_Extra_Period$Millisecond, ticks + (_rluiten$elm_date_extra$Date_Extra_Create$epochTimezoneOffset * _rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute), _rluiten$elm_date_extra$Date_Extra_Create$epochDate);
	var dateOffset = _rluiten$elm_date_extra$Date_Extra_Create$getTimezoneOffset(date);
	return _elm_lang$core$Native_Utils.eq(dateOffset, _rluiten$elm_date_extra$Date_Extra_Create$epochTimezoneOffset) ? date : A3(_rluiten$elm_date_extra$Date_Extra_Period$add, _rluiten$elm_date_extra$Date_Extra_Period$Minute, dateOffset - _rluiten$elm_date_extra$Date_Extra_Create$epochTimezoneOffset, date);
};
var _rluiten$elm_date_extra$Date_Extra_Create$dateFromFields = F7(
	function (year, month, day, hour, minute, second, millisecond) {
		return _rluiten$elm_date_extra$Date_Extra_Create$adjustedTicksToDate(
			A7(_rluiten$elm_date_extra$Date_Extra_Internal$ticksFromFields, year, month, day, hour, minute, second, millisecond));
	});
var _rluiten$elm_date_extra$Date_Extra_Create$timeFromFields = A3(_rluiten$elm_date_extra$Date_Extra_Create$dateFromFields, 1970, _elm_lang$core$Date$Jan, 1);

var _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$dayOfMonthWithSuffix = F2(
	function (pad, day) {
		var value = function () {
			var _p0 = day;
			switch (_p0) {
				case 1:
					return '1st';
				case 21:
					return '21st';
				case 2:
					return '2nd';
				case 22:
					return '22nd';
				case 3:
					return '3rd';
				case 23:
					return '23rd';
				case 31:
					return '31st';
				default:
					return A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(day),
						'th');
			}
		}();
		return pad ? A3(
			_elm_lang$core$String$padLeft,
			4,
			_elm_lang$core$Native_Utils.chr(' '),
			value) : value;
	});
var _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$monthName = function (month) {
	var _p1 = month;
	switch (_p1.ctor) {
		case 'Jan':
			return 'January';
		case 'Feb':
			return 'February';
		case 'Mar':
			return 'March';
		case 'Apr':
			return 'April';
		case 'May':
			return 'May';
		case 'Jun':
			return 'June';
		case 'Jul':
			return 'July';
		case 'Aug':
			return 'August';
		case 'Sep':
			return 'September';
		case 'Oct':
			return 'October';
		case 'Nov':
			return 'November';
		default:
			return 'December';
	}
};
var _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$monthShort = function (month) {
	var _p2 = month;
	switch (_p2.ctor) {
		case 'Jan':
			return 'Jan';
		case 'Feb':
			return 'Feb';
		case 'Mar':
			return 'Mar';
		case 'Apr':
			return 'Apr';
		case 'May':
			return 'May';
		case 'Jun':
			return 'Jun';
		case 'Jul':
			return 'Jul';
		case 'Aug':
			return 'Aug';
		case 'Sep':
			return 'Sep';
		case 'Oct':
			return 'Oct';
		case 'Nov':
			return 'Nov';
		default:
			return 'Dec';
	}
};
var _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$dayName = function (day) {
	var _p3 = day;
	switch (_p3.ctor) {
		case 'Mon':
			return 'Monday';
		case 'Tue':
			return 'Tuesday';
		case 'Wed':
			return 'Wednesday';
		case 'Thu':
			return 'Thursday';
		case 'Fri':
			return 'Friday';
		case 'Sat':
			return 'Saturday';
		default:
			return 'Sunday';
	}
};
var _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$dayShort = function (day) {
	var _p4 = day;
	switch (_p4.ctor) {
		case 'Mon':
			return 'Mon';
		case 'Tue':
			return 'Tue';
		case 'Wed':
			return 'Wed';
		case 'Thu':
			return 'Thu';
		case 'Fri':
			return 'Fri';
		case 'Sat':
			return 'Sat';
		default:
			return 'Sun';
	}
};

var _rluiten$elm_date_extra$Date_Extra_Config_Config_en_us$config = {
	i18n: {dayShort: _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$dayShort, dayName: _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$dayName, monthShort: _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$monthShort, monthName: _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$monthName, dayOfMonthWithSuffix: _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$dayOfMonthWithSuffix},
	format: {date: '%-m/%-d/%Y', longDate: '%A, %B %d, %Y', time: '%-H:%M %p', longTime: '%-H:%M:%S %p', dateTime: '%-m/%-d/%Y %-I:%M %p', firstDayOfWeek: _elm_lang$core$Date$Sun}
};

var _rluiten$elm_date_extra$Date_Extra_Format$toHourMin = function (offsetMinutes) {
	return {
		ctor: '_Tuple2',
		_0: (offsetMinutes / 60) | 0,
		_1: A2(_elm_lang$core$Basics_ops['%'], offsetMinutes, 60)
	};
};
var _rluiten$elm_date_extra$Date_Extra_Format$padWithN = F2(
	function (n, c) {
		return function (_p0) {
			return A3(
				_elm_lang$core$String$padLeft,
				n,
				c,
				_elm_lang$core$Basics$toString(_p0));
		};
	});
var _rluiten$elm_date_extra$Date_Extra_Format$padWith = function (c) {
	return function (_p1) {
		return A3(
			_elm_lang$core$String$padLeft,
			2,
			c,
			_elm_lang$core$Basics$toString(_p1));
	};
};
var _rluiten$elm_date_extra$Date_Extra_Format$hourMod12 = function (h) {
	return _elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], h, 12),
		0) ? 12 : A2(_elm_lang$core$Basics_ops['%'], h, 12);
};
var _rluiten$elm_date_extra$Date_Extra_Format$formatOffsetStr = F2(
	function (betweenHoursMinutes, offset) {
		var _p2 = _rluiten$elm_date_extra$Date_Extra_Format$toHourMin(
			_elm_lang$core$Basics$abs(offset));
		var hour = _p2._0;
		var minute = _p2._1;
		return A2(
			_elm_lang$core$Basics_ops['++'],
			(_elm_lang$core$Native_Utils.cmp(offset, 0) < 1) ? '+' : '-',
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr('0'),
					hour),
				A2(
					_elm_lang$core$Basics_ops['++'],
					betweenHoursMinutes,
					A2(
						_rluiten$elm_date_extra$Date_Extra_Format$padWith,
						_elm_lang$core$Native_Utils.chr('0'),
						minute))));
	});
var _rluiten$elm_date_extra$Date_Extra_Format$collapse = function (m) {
	return A2(_elm_lang$core$Maybe$andThen, _elm_lang$core$Basics$identity, m);
};
var _rluiten$elm_date_extra$Date_Extra_Format$formatToken = F4(
	function (config, offset, d, m) {
		var symbol = A2(
			_elm_lang$core$Maybe$withDefault,
			' ',
			_rluiten$elm_date_extra$Date_Extra_Format$collapse(
				_elm_lang$core$List$head(m.submatches)));
		var _p3 = symbol;
		switch (_p3) {
			case 'Y':
				return A3(
					_rluiten$elm_date_extra$Date_Extra_Format$padWithN,
					4,
					_elm_lang$core$Native_Utils.chr('0'),
					_elm_lang$core$Date$year(d));
			case 'y':
				return A2(
					_elm_lang$core$String$right,
					2,
					A3(
						_rluiten$elm_date_extra$Date_Extra_Format$padWithN,
						2,
						_elm_lang$core$Native_Utils.chr('0'),
						_elm_lang$core$Date$year(d)));
			case 'm':
				return A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr('0'),
					_rluiten$elm_date_extra$Date_Extra_Core$monthToInt(
						_elm_lang$core$Date$month(d)));
			case '_m':
				return A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr(' '),
					_rluiten$elm_date_extra$Date_Extra_Core$monthToInt(
						_elm_lang$core$Date$month(d)));
			case '-m':
				return _elm_lang$core$Basics$toString(
					_rluiten$elm_date_extra$Date_Extra_Core$monthToInt(
						_elm_lang$core$Date$month(d)));
			case 'B':
				return config.i18n.monthName(
					_elm_lang$core$Date$month(d));
			case '^B':
				return _elm_lang$core$String$toUpper(
					config.i18n.monthName(
						_elm_lang$core$Date$month(d)));
			case 'b':
				return config.i18n.monthShort(
					_elm_lang$core$Date$month(d));
			case '^b':
				return _elm_lang$core$String$toUpper(
					config.i18n.monthShort(
						_elm_lang$core$Date$month(d)));
			case 'd':
				return A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr('0'),
					_elm_lang$core$Date$day(d));
			case '-d':
				return _elm_lang$core$Basics$toString(
					_elm_lang$core$Date$day(d));
			case '-@d':
				return A2(
					config.i18n.dayOfMonthWithSuffix,
					false,
					_elm_lang$core$Date$day(d));
			case 'e':
				return A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr(' '),
					_elm_lang$core$Date$day(d));
			case '@e':
				return A2(
					config.i18n.dayOfMonthWithSuffix,
					true,
					_elm_lang$core$Date$day(d));
			case 'A':
				return config.i18n.dayName(
					_elm_lang$core$Date$dayOfWeek(d));
			case '^A':
				return _elm_lang$core$String$toUpper(
					config.i18n.dayName(
						_elm_lang$core$Date$dayOfWeek(d)));
			case 'a':
				return config.i18n.dayShort(
					_elm_lang$core$Date$dayOfWeek(d));
			case '^a':
				return _elm_lang$core$String$toUpper(
					config.i18n.dayShort(
						_elm_lang$core$Date$dayOfWeek(d)));
			case 'H':
				return A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr('0'),
					_elm_lang$core$Date$hour(d));
			case '-H':
				return _elm_lang$core$Basics$toString(
					_elm_lang$core$Date$hour(d));
			case 'k':
				return A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr(' '),
					_elm_lang$core$Date$hour(d));
			case 'I':
				return A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr('0'),
					_rluiten$elm_date_extra$Date_Extra_Format$hourMod12(
						_elm_lang$core$Date$hour(d)));
			case '-I':
				return _elm_lang$core$Basics$toString(
					_rluiten$elm_date_extra$Date_Extra_Format$hourMod12(
						_elm_lang$core$Date$hour(d)));
			case 'l':
				return A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr(' '),
					_rluiten$elm_date_extra$Date_Extra_Format$hourMod12(
						_elm_lang$core$Date$hour(d)));
			case 'p':
				return (_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$Date$hour(d),
					12) < 0) ? 'AM' : 'PM';
			case 'P':
				return (_elm_lang$core$Native_Utils.cmp(
					_elm_lang$core$Date$hour(d),
					12) < 0) ? 'am' : 'pm';
			case 'M':
				return A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr('0'),
					_elm_lang$core$Date$minute(d));
			case 'S':
				return A2(
					_rluiten$elm_date_extra$Date_Extra_Format$padWith,
					_elm_lang$core$Native_Utils.chr('0'),
					_elm_lang$core$Date$second(d));
			case 'L':
				return A3(
					_rluiten$elm_date_extra$Date_Extra_Format$padWithN,
					3,
					_elm_lang$core$Native_Utils.chr('0'),
					_elm_lang$core$Date$millisecond(d));
			case '%':
				return symbol;
			case 'z':
				return A2(_rluiten$elm_date_extra$Date_Extra_Format$formatOffsetStr, '', offset);
			case ':z':
				return A2(_rluiten$elm_date_extra$Date_Extra_Format$formatOffsetStr, ':', offset);
			default:
				return '';
		}
	});
var _rluiten$elm_date_extra$Date_Extra_Format$formatRegex = _elm_lang$core$Regex$regex('%(y|Y|m|_m|-m|B|^B|b|^b|d|-d|-@d|e|@e|A|^A|a|^a|H|-H|k|I|-I|l|p|P|M|S|%|L|z|:z)');
var _rluiten$elm_date_extra$Date_Extra_Format$formatOffset = F4(
	function (config, targetOffset, formatStr, date) {
		var dateOffset = _rluiten$elm_date_extra$Date_Extra_Create$getTimezoneOffset(date);
		var hackOffset = dateOffset - targetOffset;
		return A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_rluiten$elm_date_extra$Date_Extra_Format$formatRegex,
			A3(
				_rluiten$elm_date_extra$Date_Extra_Format$formatToken,
				config,
				targetOffset,
				A2(_rluiten$elm_date_extra$Date_Extra_Internal$hackDateAsOffset, hackOffset, date)),
			formatStr);
	});
var _rluiten$elm_date_extra$Date_Extra_Format$format = F3(
	function (config, formatStr, date) {
		return A4(
			_rluiten$elm_date_extra$Date_Extra_Format$formatOffset,
			config,
			_rluiten$elm_date_extra$Date_Extra_Create$getTimezoneOffset(date),
			formatStr,
			date);
	});
var _rluiten$elm_date_extra$Date_Extra_Format$formatUtc = F3(
	function (config, formatStr, date) {
		return A4(_rluiten$elm_date_extra$Date_Extra_Format$formatOffset, config, 0, formatStr, date);
	});
var _rluiten$elm_date_extra$Date_Extra_Format$isoDateString = function (date) {
	var day = _elm_lang$core$Date$day(date);
	var month = _elm_lang$core$Date$month(date);
	var year = _elm_lang$core$Date$year(date);
	return A2(
		_elm_lang$core$Basics_ops['++'],
		A3(
			_elm_lang$core$String$padLeft,
			4,
			_elm_lang$core$Native_Utils.chr('0'),
			_elm_lang$core$Basics$toString(year)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'-',
			A2(
				_elm_lang$core$Basics_ops['++'],
				A3(
					_elm_lang$core$String$padLeft,
					2,
					_elm_lang$core$Native_Utils.chr('0'),
					_elm_lang$core$Basics$toString(
						_rluiten$elm_date_extra$Date_Extra_Core$monthToInt(month))),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'-',
					A3(
						_elm_lang$core$String$padLeft,
						2,
						_elm_lang$core$Native_Utils.chr('0'),
						_elm_lang$core$Basics$toString(day))))));
};
var _rluiten$elm_date_extra$Date_Extra_Format$utcIsoDateString = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Format$isoDateString(
		_rluiten$elm_date_extra$Date_Extra_Internal$hackDateAsUtc(date));
};
var _rluiten$elm_date_extra$Date_Extra_Format$yearInt = function (year) {
	return A3(
		_elm_lang$core$String$padLeft,
		4,
		_elm_lang$core$Native_Utils.chr('0'),
		_elm_lang$core$Basics$toString(year));
};
var _rluiten$elm_date_extra$Date_Extra_Format$year = function (date) {
	return A3(
		_elm_lang$core$String$padLeft,
		4,
		_elm_lang$core$Native_Utils.chr('0'),
		_elm_lang$core$Basics$toString(
			_elm_lang$core$Date$year(date)));
};
var _rluiten$elm_date_extra$Date_Extra_Format$monthMonth = function (month) {
	return A3(
		_elm_lang$core$String$padLeft,
		2,
		_elm_lang$core$Native_Utils.chr('0'),
		_elm_lang$core$Basics$toString(
			_rluiten$elm_date_extra$Date_Extra_Core$monthToInt(month)));
};
var _rluiten$elm_date_extra$Date_Extra_Format$month = function (date) {
	return A3(
		_elm_lang$core$String$padLeft,
		2,
		_elm_lang$core$Native_Utils.chr('0'),
		_elm_lang$core$Basics$toString(
			_rluiten$elm_date_extra$Date_Extra_Core$monthToInt(
				_elm_lang$core$Date$month(date))));
};
var _rluiten$elm_date_extra$Date_Extra_Format$isoTimeFormat = '%H:%M:%S';
var _rluiten$elm_date_extra$Date_Extra_Format$isoDateFormat = '%Y-%m-%d';
var _rluiten$elm_date_extra$Date_Extra_Format$isoMsecOffsetFormat = '%Y-%m-%dT%H:%M:%S.%L%:z';
var _rluiten$elm_date_extra$Date_Extra_Format$isoString = A2(_rluiten$elm_date_extra$Date_Extra_Format$format, _rluiten$elm_date_extra$Date_Extra_Config_Config_en_us$config, _rluiten$elm_date_extra$Date_Extra_Format$isoMsecOffsetFormat);
var _rluiten$elm_date_extra$Date_Extra_Format$isoOffsetFormat = '%Y-%m-%dT%H:%M:%S%z';
var _rluiten$elm_date_extra$Date_Extra_Format$isoMsecFormat = '%Y-%m-%dT%H:%M:%S.%L';
var _rluiten$elm_date_extra$Date_Extra_Format$isoStringNoOffset = A2(_rluiten$elm_date_extra$Date_Extra_Format$format, _rluiten$elm_date_extra$Date_Extra_Config_Config_en_us$config, _rluiten$elm_date_extra$Date_Extra_Format$isoMsecFormat);
var _rluiten$elm_date_extra$Date_Extra_Format$utcIsoString = function (date) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		A3(_rluiten$elm_date_extra$Date_Extra_Format$formatUtc, _rluiten$elm_date_extra$Date_Extra_Config_Config_en_us$config, _rluiten$elm_date_extra$Date_Extra_Format$isoMsecFormat, date),
		'Z');
};
var _rluiten$elm_date_extra$Date_Extra_Format$isoFormat = '%Y-%m-%dT%H:%M:%S';

var _rluiten$elm_date_extra$Date_Extra_Compare$is3 = F4(
	function (comp, date1, date2, date3) {
		var time3 = _rluiten$elm_date_extra$Date_Extra_Core$toTime(date3);
		var time2 = _rluiten$elm_date_extra$Date_Extra_Core$toTime(date2);
		var highBound = A2(_elm_lang$core$Basics$max, time2, time3);
		var lowBound = A2(_elm_lang$core$Basics$min, time2, time3);
		var time1 = _rluiten$elm_date_extra$Date_Extra_Core$toTime(date1);
		var _p0 = comp;
		switch (_p0.ctor) {
			case 'Between':
				return (_elm_lang$core$Native_Utils.cmp(time1, lowBound) > 0) && (_elm_lang$core$Native_Utils.cmp(time1, highBound) < 0);
			case 'BetweenOpenStart':
				return (_elm_lang$core$Native_Utils.cmp(time1, lowBound) > -1) && (_elm_lang$core$Native_Utils.cmp(time1, highBound) < 0);
			case 'BetweenOpenEnd':
				return (_elm_lang$core$Native_Utils.cmp(time1, lowBound) > 0) && (_elm_lang$core$Native_Utils.cmp(time1, highBound) < 1);
			default:
				return (_elm_lang$core$Native_Utils.cmp(time1, lowBound) > -1) && (_elm_lang$core$Native_Utils.cmp(time1, highBound) < 1);
		}
	});
var _rluiten$elm_date_extra$Date_Extra_Compare$is = F3(
	function (comp, date1, date2) {
		var time2 = _rluiten$elm_date_extra$Date_Extra_Core$toTime(date2);
		var time1 = _rluiten$elm_date_extra$Date_Extra_Core$toTime(date1);
		var _p1 = comp;
		switch (_p1.ctor) {
			case 'Before':
				return _elm_lang$core$Native_Utils.cmp(time1, time2) < 0;
			case 'After':
				return _elm_lang$core$Native_Utils.cmp(time1, time2) > 0;
			case 'Same':
				return _elm_lang$core$Native_Utils.eq(time1, time2);
			case 'SameOrBefore':
				return _elm_lang$core$Native_Utils.cmp(time1, time2) < 1;
			default:
				return _elm_lang$core$Native_Utils.cmp(time1, time2) > -1;
		}
	});
var _rluiten$elm_date_extra$Date_Extra_Compare$SameOrBefore = {ctor: 'SameOrBefore'};
var _rluiten$elm_date_extra$Date_Extra_Compare$SameOrAfter = {ctor: 'SameOrAfter'};
var _rluiten$elm_date_extra$Date_Extra_Compare$Same = {ctor: 'Same'};
var _rluiten$elm_date_extra$Date_Extra_Compare$Before = {ctor: 'Before'};
var _rluiten$elm_date_extra$Date_Extra_Compare$After = {ctor: 'After'};
var _rluiten$elm_date_extra$Date_Extra_Compare$BetweenOpen = {ctor: 'BetweenOpen'};
var _rluiten$elm_date_extra$Date_Extra_Compare$BetweenOpenEnd = {ctor: 'BetweenOpenEnd'};
var _rluiten$elm_date_extra$Date_Extra_Compare$BetweenOpenStart = {ctor: 'BetweenOpenStart'};
var _rluiten$elm_date_extra$Date_Extra_Compare$Between = {ctor: 'Between'};

var _rluiten$elm_date_extra$Date_Extra_Duration$positiveDiff = F3(
	function (date1, date2, mult) {
		var accDiff = F4(
			function (acc, v1, v2, maxV2) {
				return (_elm_lang$core$Native_Utils.cmp(v1, v2) < 0) ? {ctor: '_Tuple2', _0: acc - 1, _1: (maxV2 + v1) - v2} : {ctor: '_Tuple2', _0: acc, _1: v1 - v2};
			});
		var msec2 = _elm_lang$core$Date$millisecond(date2);
		var msec1 = _elm_lang$core$Date$millisecond(date1);
		var second2 = _elm_lang$core$Date$second(date2);
		var second1 = _elm_lang$core$Date$second(date1);
		var minute2 = _elm_lang$core$Date$minute(date2);
		var minute1 = _elm_lang$core$Date$minute(date1);
		var hour2 = _elm_lang$core$Date$hour(date2);
		var hour1 = _elm_lang$core$Date$hour(date1);
		var day2 = _elm_lang$core$Date$day(date2);
		var day1 = _elm_lang$core$Date$day(date1);
		var month2Mon = _elm_lang$core$Date$month(date2);
		var month2 = _rluiten$elm_date_extra$Date_Extra_Core$monthToInt(month2Mon);
		var month1Mon = _elm_lang$core$Date$month(date1);
		var month1 = _rluiten$elm_date_extra$Date_Extra_Core$monthToInt(month1Mon);
		var year2 = _elm_lang$core$Date$year(date2);
		var daysInDate2Month = A2(_rluiten$elm_date_extra$Date_Extra_Core$daysInMonth, year2, month2Mon);
		var year1 = _elm_lang$core$Date$year(date1);
		var _p0 = A4(accDiff, year1 - year2, month1, month2, 12);
		var yearDiff = _p0._0;
		var monthDiffA = _p0._1;
		var _p1 = A4(accDiff, monthDiffA, day1, day2, daysInDate2Month);
		var monthDiff = _p1._0;
		var dayDiffA = _p1._1;
		var _p2 = A4(accDiff, dayDiffA, hour1, hour2, 24);
		var dayDiff = _p2._0;
		var hourDiffA = _p2._1;
		var _p3 = A4(accDiff, hourDiffA, minute1, minute2, 60);
		var hourDiff = _p3._0;
		var minuteDiffA = _p3._1;
		var _p4 = A4(accDiff, minuteDiffA, second1, second2, 60);
		var minuteDiff = _p4._0;
		var secondDiffA = _p4._1;
		var _p5 = A4(accDiff, secondDiffA, msec1, msec2, 1000);
		var secondDiff = _p5._0;
		var msecDiff = _p5._1;
		return {year: yearDiff * mult, month: monthDiff * mult, day: dayDiff * mult, hour: hourDiff * mult, minute: minuteDiff * mult, second: secondDiff * mult, millisecond: msecDiff * mult};
	});
var _rluiten$elm_date_extra$Date_Extra_Duration$diff = F2(
	function (date1, date2) {
		return A3(_rluiten$elm_date_extra$Date_Extra_Compare$is, _rluiten$elm_date_extra$Date_Extra_Compare$After, date1, date2) ? A3(_rluiten$elm_date_extra$Date_Extra_Duration$positiveDiff, date1, date2, 1) : A3(_rluiten$elm_date_extra$Date_Extra_Duration$positiveDiff, date2, date1, -1);
	});
var _rluiten$elm_date_extra$Date_Extra_Duration$addMonth = F2(
	function (monthCount, date) {
		var day = _elm_lang$core$Date$day(date);
		var monthInt = _rluiten$elm_date_extra$Date_Extra_Core$monthToInt(
			_elm_lang$core$Date$month(date));
		var newMonthInt = monthInt + monthCount;
		var targetMonthInt = A2(_elm_lang$core$Basics_ops['%'], newMonthInt, 12);
		var yearOffset = (_elm_lang$core$Native_Utils.cmp(newMonthInt, 0) < 0) ? (((newMonthInt / 12) | 0) - 1) : ((newMonthInt / 12) | 0);
		var year = _elm_lang$core$Date$year(date);
		var inputCivil = A3(_rluiten$elm_date_extra$Date_Extra_Internal$daysFromCivil, year, monthInt, day);
		var newYear = year + yearOffset;
		var newDay = A2(
			_elm_lang$core$Basics$min,
			A2(
				_rluiten$elm_date_extra$Date_Extra_Core$daysInMonth,
				newYear,
				_rluiten$elm_date_extra$Date_Extra_Core$intToMonth(newMonthInt)),
			day);
		var newCivil = A3(_rluiten$elm_date_extra$Date_Extra_Internal$daysFromCivil, newYear, targetMonthInt, newDay);
		var daysDifferent = newCivil - inputCivil;
		return A3(_rluiten$elm_date_extra$Date_Extra_Period$add, _rluiten$elm_date_extra$Date_Extra_Period$Day, daysDifferent, date);
	});
var _rluiten$elm_date_extra$Date_Extra_Duration$addYear = F2(
	function (yearCount, date) {
		return A2(_rluiten$elm_date_extra$Date_Extra_Duration$addMonth, 12 * yearCount, date);
	});
var _rluiten$elm_date_extra$Date_Extra_Duration$daylightOffsetCompensate = F2(
	function (dateBefore, dateAfter) {
		var offsetAfter = _rluiten$elm_date_extra$Date_Extra_Create$getTimezoneOffset(dateAfter);
		var offsetBefore = _rluiten$elm_date_extra$Date_Extra_Create$getTimezoneOffset(dateBefore);
		if (!_elm_lang$core$Native_Utils.eq(offsetBefore, offsetAfter)) {
			var adjustedDate = A3(_rluiten$elm_date_extra$Date_Extra_Period$add, _rluiten$elm_date_extra$Date_Extra_Period$Millisecond, (offsetAfter - offsetBefore) * _rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute, dateAfter);
			var adjustedOffset = _rluiten$elm_date_extra$Date_Extra_Create$getTimezoneOffset(adjustedDate);
			return (!_elm_lang$core$Native_Utils.eq(adjustedOffset, offsetAfter)) ? dateAfter : adjustedDate;
		} else {
			return dateAfter;
		}
	});
var _rluiten$elm_date_extra$Date_Extra_Duration$requireDaylightCompensateInAdd = function (duration) {
	var _p6 = duration;
	switch (_p6.ctor) {
		case 'Millisecond':
			return false;
		case 'Second':
			return false;
		case 'Minute':
			return false;
		case 'Hour':
			return false;
		case 'Day':
			return true;
		case 'Week':
			return true;
		case 'Month':
			return true;
		case 'Year':
			return true;
		default:
			var _p7 = _p6._0;
			return (!_elm_lang$core$Native_Utils.eq(_p7.day, 0)) || ((!_elm_lang$core$Native_Utils.eq(_p7.month, 0)) || (!_elm_lang$core$Native_Utils.eq(_p7.year, 0)));
	}
};
var _rluiten$elm_date_extra$Date_Extra_Duration$zeroDelta = {year: 0, month: 0, day: 0, hour: 0, minute: 0, second: 0, millisecond: 0};
var _rluiten$elm_date_extra$Date_Extra_Duration$DeltaRecord = F7(
	function (a, b, c, d, e, f, g) {
		return {year: a, month: b, day: c, hour: d, minute: e, second: f, millisecond: g};
	});
var _rluiten$elm_date_extra$Date_Extra_Duration$Delta = function (a) {
	return {ctor: 'Delta', _0: a};
};
var _rluiten$elm_date_extra$Date_Extra_Duration$Year = {ctor: 'Year'};
var _rluiten$elm_date_extra$Date_Extra_Duration$Month = {ctor: 'Month'};
var _rluiten$elm_date_extra$Date_Extra_Duration$doAdd = F3(
	function (duration, addend, date) {
		var _p8 = duration;
		switch (_p8.ctor) {
			case 'Millisecond':
				return A3(_rluiten$elm_date_extra$Date_Extra_Period$add, _rluiten$elm_date_extra$Date_Extra_Period$Millisecond, addend, date);
			case 'Second':
				return A3(_rluiten$elm_date_extra$Date_Extra_Period$add, _rluiten$elm_date_extra$Date_Extra_Period$Second, addend, date);
			case 'Minute':
				return A3(_rluiten$elm_date_extra$Date_Extra_Period$add, _rluiten$elm_date_extra$Date_Extra_Period$Minute, addend, date);
			case 'Hour':
				return A3(_rluiten$elm_date_extra$Date_Extra_Period$add, _rluiten$elm_date_extra$Date_Extra_Period$Hour, addend, date);
			case 'Day':
				return A3(_rluiten$elm_date_extra$Date_Extra_Period$add, _rluiten$elm_date_extra$Date_Extra_Period$Day, addend, date);
			case 'Week':
				return A3(_rluiten$elm_date_extra$Date_Extra_Period$add, _rluiten$elm_date_extra$Date_Extra_Period$Week, addend, date);
			case 'Month':
				return A2(_rluiten$elm_date_extra$Date_Extra_Duration$addMonth, addend, date);
			case 'Year':
				return A2(_rluiten$elm_date_extra$Date_Extra_Duration$addYear, addend, date);
			default:
				var _p9 = _p8._0;
				return A3(
					_rluiten$elm_date_extra$Date_Extra_Period$add,
					_rluiten$elm_date_extra$Date_Extra_Period$Delta(
						{week: 0, day: _p9.day, hour: _p9.hour, minute: _p9.minute, second: _p9.second, millisecond: _p9.millisecond}),
					addend,
					A3(
						_rluiten$elm_date_extra$Date_Extra_Duration$doAdd,
						_rluiten$elm_date_extra$Date_Extra_Duration$Month,
						_p9.month,
						A3(_rluiten$elm_date_extra$Date_Extra_Duration$doAdd, _rluiten$elm_date_extra$Date_Extra_Duration$Year, _p9.year, date)));
		}
	});
var _rluiten$elm_date_extra$Date_Extra_Duration$add = F3(
	function (duration, addend, date) {
		var outputDate = A3(_rluiten$elm_date_extra$Date_Extra_Duration$doAdd, duration, addend, date);
		return _rluiten$elm_date_extra$Date_Extra_Duration$requireDaylightCompensateInAdd(duration) ? A2(_rluiten$elm_date_extra$Date_Extra_Duration$daylightOffsetCompensate, date, outputDate) : outputDate;
	});
var _rluiten$elm_date_extra$Date_Extra_Duration$Week = {ctor: 'Week'};
var _rluiten$elm_date_extra$Date_Extra_Duration$Day = {ctor: 'Day'};
var _rluiten$elm_date_extra$Date_Extra_Duration$Hour = {ctor: 'Hour'};
var _rluiten$elm_date_extra$Date_Extra_Duration$Minute = {ctor: 'Minute'};
var _rluiten$elm_date_extra$Date_Extra_Duration$Second = {ctor: 'Second'};
var _rluiten$elm_date_extra$Date_Extra_Duration$Millisecond = {ctor: 'Millisecond'};

var _rluiten$elm_date_extra$Date_Extra_Field$dayOfWeekToDate = F3(
	function (newDayOfWeek, startOfWeekDay, date) {
		var targetIsoDay = _rluiten$elm_date_extra$Date_Extra_Core$isoDayOfWeek(newDayOfWeek);
		var dayOfWeek = _elm_lang$core$Date$dayOfWeek(date);
		var daysToStartOfWeek = A2(_rluiten$elm_date_extra$Date_Extra_Core$daysBackToStartOfWeek, dayOfWeek, startOfWeekDay);
		var isoDay = _rluiten$elm_date_extra$Date_Extra_Core$isoDayOfWeek(dayOfWeek);
		var dayDiff = targetIsoDay - isoDay;
		var adjustedDiff = (_elm_lang$core$Native_Utils.cmp(daysToStartOfWeek + dayDiff, 0) < 0) ? (dayDiff + 7) : dayDiff;
		return A3(_rluiten$elm_date_extra$Date_Extra_Duration$add, _rluiten$elm_date_extra$Date_Extra_Duration$Day, adjustedDiff, date);
	});
var _rluiten$elm_date_extra$Date_Extra_Field$monthToDate = F2(
	function (month, date) {
		var monthInt = _rluiten$elm_date_extra$Date_Extra_Core$monthToInt(
			_elm_lang$core$Date$month(date));
		var targetMonthInt = _rluiten$elm_date_extra$Date_Extra_Core$monthToInt(month);
		return A3(_rluiten$elm_date_extra$Date_Extra_Duration$add, _rluiten$elm_date_extra$Date_Extra_Duration$Month, targetMonthInt - monthInt, date);
	});
var _rluiten$elm_date_extra$Date_Extra_Field$fieldToDateClamp = F2(
	function (field, date) {
		var _p0 = field;
		switch (_p0.ctor) {
			case 'Millisecond':
				return A3(
					_rluiten$elm_date_extra$Date_Extra_Duration$add,
					_rluiten$elm_date_extra$Date_Extra_Duration$Millisecond,
					A3(_elm_lang$core$Basics$clamp, 0, 999, _p0._0) - _elm_lang$core$Date$millisecond(date),
					date);
			case 'Second':
				return A3(
					_rluiten$elm_date_extra$Date_Extra_Duration$add,
					_rluiten$elm_date_extra$Date_Extra_Duration$Second,
					A3(_elm_lang$core$Basics$clamp, 0, 59, _p0._0) - _elm_lang$core$Date$second(date),
					date);
			case 'Minute':
				return A3(
					_rluiten$elm_date_extra$Date_Extra_Duration$add,
					_rluiten$elm_date_extra$Date_Extra_Duration$Minute,
					A3(_elm_lang$core$Basics$clamp, 0, 59, _p0._0) - _elm_lang$core$Date$minute(date),
					date);
			case 'Hour':
				return A3(
					_rluiten$elm_date_extra$Date_Extra_Duration$add,
					_rluiten$elm_date_extra$Date_Extra_Duration$Hour,
					A3(_elm_lang$core$Basics$clamp, 0, 23, _p0._0) - _elm_lang$core$Date$hour(date),
					date);
			case 'DayOfWeek':
				return A3(_rluiten$elm_date_extra$Date_Extra_Field$dayOfWeekToDate, _p0._0._0, _p0._0._1, date);
			case 'DayOfMonth':
				var maxDays = _rluiten$elm_date_extra$Date_Extra_Core$daysInMonthDate(date);
				return A3(
					_rluiten$elm_date_extra$Date_Extra_Duration$add,
					_rluiten$elm_date_extra$Date_Extra_Duration$Day,
					A3(_elm_lang$core$Basics$clamp, 1, maxDays, _p0._0) - _elm_lang$core$Date$day(date),
					date);
			case 'Month':
				return A2(_rluiten$elm_date_extra$Date_Extra_Field$monthToDate, _p0._0, date);
			default:
				var _p1 = _p0._0;
				var minYear = (_elm_lang$core$Native_Utils.cmp(_p1, 0) < 0) ? 0 : _p1;
				return A3(
					_rluiten$elm_date_extra$Date_Extra_Duration$add,
					_rluiten$elm_date_extra$Date_Extra_Duration$Year,
					minYear - _elm_lang$core$Date$year(date),
					date);
		}
	});
var _rluiten$elm_date_extra$Date_Extra_Field$fieldToDate = F2(
	function (field, date) {
		var _p2 = field;
		switch (_p2.ctor) {
			case 'Millisecond':
				var _p3 = _p2._0;
				return ((_elm_lang$core$Native_Utils.cmp(_p3, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(_p3, 999) > 0)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
					A3(
						_rluiten$elm_date_extra$Date_Extra_Duration$add,
						_rluiten$elm_date_extra$Date_Extra_Duration$Millisecond,
						_p3 - _elm_lang$core$Date$millisecond(date),
						date));
			case 'Second':
				var _p4 = _p2._0;
				return ((_elm_lang$core$Native_Utils.cmp(_p4, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(_p4, 59) > 0)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
					A3(
						_rluiten$elm_date_extra$Date_Extra_Duration$add,
						_rluiten$elm_date_extra$Date_Extra_Duration$Second,
						_p4 - _elm_lang$core$Date$second(date),
						date));
			case 'Minute':
				var _p5 = _p2._0;
				return ((_elm_lang$core$Native_Utils.cmp(_p5, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(_p5, 59) > 0)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
					A3(
						_rluiten$elm_date_extra$Date_Extra_Duration$add,
						_rluiten$elm_date_extra$Date_Extra_Duration$Minute,
						_p5 - _elm_lang$core$Date$minute(date),
						date));
			case 'Hour':
				var _p6 = _p2._0;
				return ((_elm_lang$core$Native_Utils.cmp(_p6, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(_p6, 23) > 0)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
					A3(
						_rluiten$elm_date_extra$Date_Extra_Duration$add,
						_rluiten$elm_date_extra$Date_Extra_Duration$Hour,
						_p6 - _elm_lang$core$Date$hour(date),
						date));
			case 'DayOfWeek':
				return _elm_lang$core$Maybe$Just(
					A3(_rluiten$elm_date_extra$Date_Extra_Field$dayOfWeekToDate, _p2._0._0, _p2._0._1, date));
			case 'DayOfMonth':
				var _p7 = _p2._0;
				var maxDays = _rluiten$elm_date_extra$Date_Extra_Core$daysInMonthDate(date);
				return ((_elm_lang$core$Native_Utils.cmp(_p7, 1) < 0) || (_elm_lang$core$Native_Utils.cmp(_p7, maxDays) > 0)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
					A3(
						_rluiten$elm_date_extra$Date_Extra_Duration$add,
						_rluiten$elm_date_extra$Date_Extra_Duration$Day,
						_p7 - _elm_lang$core$Date$day(date),
						date));
			case 'Month':
				return _elm_lang$core$Maybe$Just(
					A2(_rluiten$elm_date_extra$Date_Extra_Field$monthToDate, _p2._0, date));
			default:
				var _p8 = _p2._0;
				return (_elm_lang$core$Native_Utils.cmp(_p8, 0) < 0) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
					A3(
						_rluiten$elm_date_extra$Date_Extra_Duration$add,
						_rluiten$elm_date_extra$Date_Extra_Duration$Year,
						_p8 - _elm_lang$core$Date$year(date),
						date));
		}
	});
var _rluiten$elm_date_extra$Date_Extra_Field$Year = function (a) {
	return {ctor: 'Year', _0: a};
};
var _rluiten$elm_date_extra$Date_Extra_Field$Month = function (a) {
	return {ctor: 'Month', _0: a};
};
var _rluiten$elm_date_extra$Date_Extra_Field$DayOfMonth = function (a) {
	return {ctor: 'DayOfMonth', _0: a};
};
var _rluiten$elm_date_extra$Date_Extra_Field$DayOfWeek = function (a) {
	return {ctor: 'DayOfWeek', _0: a};
};
var _rluiten$elm_date_extra$Date_Extra_Field$Hour = function (a) {
	return {ctor: 'Hour', _0: a};
};
var _rluiten$elm_date_extra$Date_Extra_Field$Minute = function (a) {
	return {ctor: 'Minute', _0: a};
};
var _rluiten$elm_date_extra$Date_Extra_Field$Second = function (a) {
	return {ctor: 'Second', _0: a};
};
var _rluiten$elm_date_extra$Date_Extra_Field$Millisecond = function (a) {
	return {ctor: 'Millisecond', _0: a};
};

var _rluiten$elm_date_extra$Date_Extra_Config_Config_en_au$config = {
	i18n: {dayShort: _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$dayShort, dayName: _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$dayName, monthShort: _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$monthShort, monthName: _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$monthName, dayOfMonthWithSuffix: _rluiten$elm_date_extra$Date_Extra_I18n_I_en_us$dayOfMonthWithSuffix},
	format: {date: '%-d/%m/%Y', longDate: '%A, %-d %B %Y', time: '%-I:%M %p', longTime: '%-I:%M:%S %p', dateTime: '%-d/%m/%Y %-I:%M %p', firstDayOfWeek: _elm_lang$core$Date$Mon}
};

var _Leonti$elm_material_datepicker$DatePicker$chunks = F2(
	function (k, xs) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(xs),
			k) > 0) ? {
			ctor: '::',
			_0: A2(_elm_lang$core$List$take, k, xs),
			_1: A2(
				_Leonti$elm_material_datepicker$DatePicker$chunks,
				k,
				A2(_elm_lang$core$List$drop, k, xs))
		} : {
			ctor: '::',
			_0: xs,
			_1: {ctor: '[]'}
		};
	});
var _Leonti$elm_material_datepicker$DatePicker$weekDays = function () {
	var days = A2(
		_elm_lang$core$List$map,
		function (day) {
			return A2(
				_elm_lang$html$Html$span,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(day),
					_1: {ctor: '[]'}
				});
		},
		{
			ctor: '::',
			_0: 'M',
			_1: {
				ctor: '::',
				_0: 'T',
				_1: {
					ctor: '::',
					_0: 'W',
					_1: {
						ctor: '::',
						_0: 'T',
						_1: {
							ctor: '::',
							_0: 'F',
							_1: {
								ctor: '::',
								_0: 'S',
								_1: {
									ctor: '::',
									_0: 'S',
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			}
		});
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('week-days'),
			_1: {ctor: '[]'}
		},
		days);
}();
var _Leonti$elm_material_datepicker$DatePicker$update = F2(
	function (msg, model) {
		var _p0 = msg;
		switch (_p0.ctor) {
			case 'Noop':
				return model;
			case 'YearSelection':
				return _elm_lang$core$Native_Utils.update(
					model,
					{selectingYear: true});
			case 'DaySelection':
				return _elm_lang$core$Native_Utils.update(
					model,
					{selectingYear: false});
			case 'PrevMonth':
				return _elm_lang$core$Native_Utils.update(
					model,
					{
						date: A3(_rluiten$elm_date_extra$Date_Extra_Duration$add, _rluiten$elm_date_extra$Date_Extra_Duration$Month, -1, model.date)
					});
			case 'NextMonth':
				return _elm_lang$core$Native_Utils.update(
					model,
					{
						date: A3(_rluiten$elm_date_extra$Date_Extra_Duration$add, _rluiten$elm_date_extra$Date_Extra_Duration$Month, 1, model.date)
					});
			case 'SelectYear':
				var _p1 = A2(
					_rluiten$elm_date_extra$Date_Extra_Field$fieldToDate,
					_rluiten$elm_date_extra$Date_Extra_Field$Year(_p0._0),
					model.date);
				if (_p1.ctor === 'Just') {
					return _elm_lang$core$Native_Utils.update(
						model,
						{date: _p1._0, selectingYear: false});
				} else {
					return model;
				}
			default:
				var _p2 = A2(
					_rluiten$elm_date_extra$Date_Extra_Field$fieldToDate,
					_rluiten$elm_date_extra$Date_Extra_Field$DayOfMonth(_p0._0),
					model.date);
				if (_p2.ctor === 'Just') {
					return _elm_lang$core$Native_Utils.update(
						model,
						{date: _p2._0});
				} else {
					return model;
				}
		}
	});
var _Leonti$elm_material_datepicker$DatePicker$formattedMonth = function (model) {
	return A3(_rluiten$elm_date_extra$Date_Extra_Format$format, _rluiten$elm_date_extra$Date_Extra_Config_Config_en_au$config, '%B %Y', model.date);
};
var _Leonti$elm_material_datepicker$DatePicker$formattedDay = function (model) {
	return A3(_rluiten$elm_date_extra$Date_Extra_Format$format, _rluiten$elm_date_extra$Date_Extra_Config_Config_en_au$config, '%a, %b %-d', model.date);
};
var _Leonti$elm_material_datepicker$DatePicker$selectedDate = function (model) {
	return model.date;
};
var _Leonti$elm_material_datepicker$DatePicker$init = F2(
	function (date, mainColor) {
		return {date: date, selectingYear: false, mainColor: mainColor};
	});
var _Leonti$elm_material_datepicker$DatePicker$Model = F3(
	function (a, b, c) {
		return {date: a, selectingYear: b, mainColor: c};
	});
var _Leonti$elm_material_datepicker$DatePicker$SelectDay = function (a) {
	return {ctor: 'SelectDay', _0: a};
};
var _Leonti$elm_material_datepicker$DatePicker$dayCell = F3(
	function (dayNumber, currentDay, mainColor) {
		if (_elm_lang$core$Native_Utils.cmp(dayNumber, 0) > 0) {
			var backgroundClass = _elm_lang$core$Native_Utils.eq(dayNumber, currentDay) ? 'day-background selected' : 'day-background';
			return A2(
				_elm_lang$html$Html$button,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('day'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onClick(
							_Leonti$elm_material_datepicker$DatePicker$SelectDay(dayNumber)),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class(backgroundClass),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$style(
									{
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'background-color', _1: mainColor},
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('day-number'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(
									_elm_lang$core$Basics$toString(dayNumber)),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				});
		} else {
			return A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('empty-day'),
					_1: {ctor: '[]'}
				},
				{ctor: '[]'});
		}
	});
var _Leonti$elm_material_datepicker$DatePicker$weekRow = F3(
	function (days, currentDay, mainColor) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('days-row'),
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$List$map,
				function (day) {
					return A3(_Leonti$elm_material_datepicker$DatePicker$dayCell, day, currentDay, mainColor);
				},
				days));
	});
var _Leonti$elm_material_datepicker$DatePicker$monthDays = function (model) {
	var weekDay = _rluiten$elm_date_extra$Date_Extra_Core$isoDayOfWeek(
		_elm_lang$core$Date$dayOfWeek(
			_rluiten$elm_date_extra$Date_Extra_Core$toFirstOfMonth(model.date)));
	var leftPadding = weekDay - 1;
	var daysCount = A2(
		_rluiten$elm_date_extra$Date_Extra_Core$daysInMonth,
		_elm_lang$core$Date$year(model.date),
		_elm_lang$core$Date$month(model.date));
	var rightPadding = (35 - daysCount) - leftPadding;
	var weeks = A2(
		_Leonti$elm_material_datepicker$DatePicker$chunks,
		7,
		A2(
			_elm_lang$core$Basics_ops['++'],
			A2(_elm_lang$core$List$repeat, leftPadding, 0),
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_elm_lang$core$List$range, 1, daysCount),
				A2(_elm_lang$core$List$repeat, rightPadding, 0))));
	var rows = A2(
		_elm_lang$core$List$map,
		function (week) {
			return A3(
				_Leonti$elm_material_datepicker$DatePicker$weekRow,
				week,
				_elm_lang$core$Date$day(model.date),
				model.mainColor);
		},
		weeks);
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('month-days'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('day-rows'),
					_1: {ctor: '[]'}
				},
				rows),
			_1: {ctor: '[]'}
		});
};
var _Leonti$elm_material_datepicker$DatePicker$SelectYear = function (a) {
	return {ctor: 'SelectYear', _0: a};
};
var _Leonti$elm_material_datepicker$DatePicker$yearButton = F2(
	function (year, currentYear) {
		var spanClass = _elm_lang$core$Native_Utils.eq(year, currentYear) ? 'selected' : '';
		return A2(
			_elm_lang$html$Html$button,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('year'),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onClick(
						_Leonti$elm_material_datepicker$DatePicker$SelectYear(year)),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$span,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class(spanClass),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_elm_lang$core$Basics$toString(year)),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	});
var _Leonti$elm_material_datepicker$DatePicker$yearPicker = function (model) {
	var yearButtons = A2(
		_elm_lang$core$List$map,
		function (y) {
			return A2(
				_Leonti$elm_material_datepicker$DatePicker$yearButton,
				y,
				_elm_lang$core$Date$year(model.date));
		},
		A2(_elm_lang$core$List$range, 1917, 2117));
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('year-picker'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('year-list-wrapper'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('year-list'),
							_1: {ctor: '[]'}
						},
						yearButtons),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _Leonti$elm_material_datepicker$DatePicker$NextMonth = {ctor: 'NextMonth'};
var _Leonti$elm_material_datepicker$DatePicker$PrevMonth = {ctor: 'PrevMonth'};
var _Leonti$elm_material_datepicker$DatePicker$picker = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('picker'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('month-year-selector'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$button,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('navigation-wrapper'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Events$onClick(_Leonti$elm_material_datepicker$DatePicker$PrevMonth),
								_1: {ctor: '[]'}
							}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$svg$Svg$svg,
								{
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$class('navigation-icon'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$viewBox('0 0 24 24'),
										_1: {ctor: '[]'}
									}
								},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$svg$Svg$path,
										{
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$d('M15.41 7.41L14 6l-6 6 6 6 1.41-1.41L10.83 12z'),
											_1: {ctor: '[]'}
										},
										{ctor: '[]'}),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('month-year'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(
									_Leonti$elm_material_datepicker$DatePicker$formattedMonth(model)),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$button,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('navigation-wrapper'),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Events$onClick(_Leonti$elm_material_datepicker$DatePicker$NextMonth),
										_1: {ctor: '[]'}
									}
								},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$svg$Svg$svg,
										{
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$class('navigation-icon'),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$viewBox('0 0 24 24'),
												_1: {ctor: '[]'}
											}
										},
										{
											ctor: '::',
											_0: A2(
												_elm_lang$svg$Svg$path,
												{
													ctor: '::',
													_0: _elm_lang$svg$Svg_Attributes$d('M10 6L8.59 7.41 13.17 12l-4.58 4.59L10 18l6-6z'),
													_1: {ctor: '[]'}
												},
												{ctor: '[]'}),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}
				}),
			_1: {
				ctor: '::',
				_0: _Leonti$elm_material_datepicker$DatePicker$weekDays,
				_1: {
					ctor: '::',
					_0: _Leonti$elm_material_datepicker$DatePicker$monthDays(model),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _Leonti$elm_material_datepicker$DatePicker$DaySelection = {ctor: 'DaySelection'};
var _Leonti$elm_material_datepicker$DatePicker$YearSelection = {ctor: 'YearSelection'};
var _Leonti$elm_material_datepicker$DatePicker$header = function (model) {
	var _p3 = model.selectingYear ? {ctor: '_Tuple2', _0: 'day', _1: 'year selected'} : {ctor: '_Tuple2', _0: 'day selected', _1: 'year'};
	var dayClass = _p3._0;
	var yearClass = _p3._1;
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('header'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'background-color', _1: model.mainColor},
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class(yearClass),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onClick(_Leonti$elm_material_datepicker$DatePicker$YearSelection),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(
						_elm_lang$core$Basics$toString(
							_elm_lang$core$Date$year(model.date))),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class(dayClass),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onClick(_Leonti$elm_material_datepicker$DatePicker$DaySelection),
							_1: {ctor: '[]'}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_Leonti$elm_material_datepicker$DatePicker$formattedDay(model)),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _Leonti$elm_material_datepicker$DatePicker$view = function (model) {
	var content = model.selectingYear ? _Leonti$elm_material_datepicker$DatePicker$yearPicker(model) : _Leonti$elm_material_datepicker$DatePicker$picker(model);
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('date-picker'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _Leonti$elm_material_datepicker$DatePicker$header(model),
			_1: {
				ctor: '::',
				_0: content,
				_1: {ctor: '[]'}
			}
		});
};
var _Leonti$elm_material_datepicker$DatePicker$Noop = {ctor: 'Noop'};

var _Leonti$elm_time_picker$TimeTypes$Selection = F2(
	function (a, b) {
		return {angle: a, isInner: b};
	});
var _Leonti$elm_time_picker$TimeTypes$Minutes = {ctor: 'Minutes'};
var _Leonti$elm_time_picker$TimeTypes$Hours = {ctor: 'Hours'};
var _Leonti$elm_time_picker$TimeTypes$PM = {ctor: 'PM'};
var _Leonti$elm_time_picker$TimeTypes$AM = {ctor: 'AM'};

var _Leonti$elm_time_picker$CalculatedModel$hoursInner = {
	ctor: '::',
	_0: 0,
	_1: A2(_elm_lang$core$List$range, 13, 23)
};
var _Leonti$elm_time_picker$CalculatedModel$hoursOuter = {
	ctor: '::',
	_0: 12,
	_1: A2(_elm_lang$core$List$range, 1, 11)
};
var _Leonti$elm_time_picker$CalculatedModel$applySelection24h = F2(
	function (model, selection) {
		var _p0 = selection.isInner;
		if (_p0 === true) {
			var maybeHour = _elm_lang$core$List$head(
				A2(_elm_lang$core$List$drop, selection.angle, _Leonti$elm_time_picker$CalculatedModel$hoursInner));
			var _p1 = maybeHour;
			if (_p1.ctor === 'Just') {
				return {hoursSelected: _p1._0, minutesSelected: model.minutesSelected};
			} else {
				return {hoursSelected: model.hoursSelected, minutesSelected: model.minutesSelected};
			}
		} else {
			var maybeHour = _elm_lang$core$List$head(
				A2(_elm_lang$core$List$drop, selection.angle, _Leonti$elm_time_picker$CalculatedModel$hoursOuter));
			var _p2 = maybeHour;
			if (_p2.ctor === 'Just') {
				return {hoursSelected: _p2._0, minutesSelected: model.minutesSelected};
			} else {
				return {hoursSelected: model.hoursSelected, minutesSelected: model.minutesSelected};
			}
		}
	});
var _Leonti$elm_time_picker$CalculatedModel$hoursToTimePeriod = function (hours) {
	return ((_elm_lang$core$Native_Utils.cmp(hours, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(hours, 12) < 0)) ? _Leonti$elm_time_picker$TimeTypes$AM : _Leonti$elm_time_picker$TimeTypes$PM;
};
var _Leonti$elm_time_picker$CalculatedModel$isInnerSelected = function (selected) {
	return A2(
		_elm_lang$core$List$member,
		selected,
		{
			ctor: '::',
			_0: 0,
			_1: A2(_elm_lang$core$List$range, 13, 23)
		});
};
var _Leonti$elm_time_picker$CalculatedModel$pointerAngle = F2(
	function (base, value) {
		var angle = _elm_lang$core$Basics$round(
			(360 / _elm_lang$core$Basics$toFloat(base)) * _elm_lang$core$Basics$toFloat(value));
		return _elm_lang$core$Native_Utils.eq(angle, 360) ? 0 : angle;
	});
var _Leonti$elm_time_picker$CalculatedModel$pointerAngle12 = _Leonti$elm_time_picker$CalculatedModel$pointerAngle(12);
var _Leonti$elm_time_picker$CalculatedModel$pointerAngle60 = _Leonti$elm_time_picker$CalculatedModel$pointerAngle(60);
var _Leonti$elm_time_picker$CalculatedModel$isAm = function (hours) {
	return ((_elm_lang$core$Native_Utils.cmp(hours, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(hours, 12) < 0)) ? true : false;
};
var _Leonti$elm_time_picker$CalculatedModel$toAmHours = function (hours) {
	return (_elm_lang$core$Native_Utils.eq(hours, 0) || _elm_lang$core$Native_Utils.eq(hours, 12)) ? 12 : (_Leonti$elm_time_picker$CalculatedModel$isAm(hours) ? hours : (hours - 12));
};
var _Leonti$elm_time_picker$CalculatedModel$toPmHours = function (hours) {
	return _Leonti$elm_time_picker$CalculatedModel$isAm(hours) ? (hours + 12) : hours;
};
var _Leonti$elm_time_picker$CalculatedModel$applySelection12h = F2(
	function (model, selection) {
		var maybeHour = _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, selection.angle, _Leonti$elm_time_picker$CalculatedModel$hoursOuter));
		var _p3 = maybeHour;
		if (_p3.ctor === 'Just') {
			var _p4 = _p3._0;
			if (_Leonti$elm_time_picker$CalculatedModel$isAm(model.hoursSelected)) {
				var adjustedHour = _elm_lang$core$Native_Utils.eq(_p4, 12) ? 0 : _p4;
				return {hoursSelected: adjustedHour, minutesSelected: model.minutesSelected};
			} else {
				var pmHour = _p4 + 12;
				var adjustedPmHour = _elm_lang$core$Native_Utils.eq(pmHour, 24) ? 12 : pmHour;
				return {hoursSelected: adjustedPmHour, minutesSelected: model.minutesSelected};
			}
		} else {
			return {hoursSelected: model.hoursSelected, minutesSelected: model.minutesSelected};
		}
	});
var _Leonti$elm_time_picker$CalculatedModel$toSelectedTime = F2(
	function (model, selection) {
		var _p5 = model.mode;
		if (_p5.ctor === 'Minutes') {
			var minutesRange = A2(_elm_lang$core$List$range, 0, 59);
			var maybeMinute = _elm_lang$core$List$head(
				A2(_elm_lang$core$List$drop, selection.angle, minutesRange));
			var _p6 = maybeMinute;
			if (_p6.ctor === 'Just') {
				return {hoursSelected: model.hoursSelected, minutesSelected: _p6._0};
			} else {
				return {hoursSelected: model.hoursSelected, minutesSelected: model.minutesSelected};
			}
		} else {
			var _p7 = model.is24Hours;
			if (_p7 === true) {
				return A2(_Leonti$elm_time_picker$CalculatedModel$applySelection24h, model, selection);
			} else {
				return A2(_Leonti$elm_time_picker$CalculatedModel$applySelection12h, model, selection);
			}
		}
	});
var _Leonti$elm_time_picker$CalculatedModel$toPointerAngle = F4(
	function (mode, is24Hours, hours, minutes) {
		var _p8 = mode;
		if (_p8.ctor === 'Hours') {
			return _Leonti$elm_time_picker$CalculatedModel$pointerAngle12(
				_Leonti$elm_time_picker$CalculatedModel$toAmHours(hours));
		} else {
			return _Leonti$elm_time_picker$CalculatedModel$pointerAngle60(minutes);
		}
	});
var _Leonti$elm_time_picker$CalculatedModel$doubleDigitFormat = function (number) {
	return (_elm_lang$core$Native_Utils.cmp(number, 10) < 0) ? A2(
		_elm_lang$core$Basics_ops['++'],
		'0',
		_elm_lang$core$Basics$toString(number)) : _elm_lang$core$Basics$toString(number);
};
var _Leonti$elm_time_picker$CalculatedModel$digitalTimeHoursToDisplay = F2(
	function (is24Hours, hours) {
		return is24Hours ? _Leonti$elm_time_picker$CalculatedModel$doubleDigitFormat(hours) : _Leonti$elm_time_picker$CalculatedModel$doubleDigitFormat(
			_Leonti$elm_time_picker$CalculatedModel$toAmHours(hours));
	});
var _Leonti$elm_time_picker$CalculatedModel$toDoubleZeroString = function (number) {
	return _elm_lang$core$Native_Utils.eq(number, 0) ? '00' : _elm_lang$core$Basics$toString(number);
};
var _Leonti$elm_time_picker$CalculatedModel$toSelectedNumber = F4(
	function (mode, is24Hours, hours, minutes) {
		var _p9 = mode;
		if (_p9.ctor === 'Hours') {
			return is24Hours ? _Leonti$elm_time_picker$CalculatedModel$toDoubleZeroString(hours) : _Leonti$elm_time_picker$CalculatedModel$toDoubleZeroString(
				_Leonti$elm_time_picker$CalculatedModel$toAmHours(hours));
		} else {
			return _Leonti$elm_time_picker$CalculatedModel$doubleDigitFormat(minutes);
		}
	});
var _Leonti$elm_time_picker$CalculatedModel$innerNumbersToDisplay = F2(
	function (mode, is24Hours) {
		var _p10 = mode;
		if (_p10.ctor === 'Hours') {
			var _p11 = is24Hours;
			if (_p11 === true) {
				return _elm_lang$core$Maybe$Just(
					A2(
						_elm_lang$core$List$map,
						_Leonti$elm_time_picker$CalculatedModel$toDoubleZeroString,
						{
							ctor: '::',
							_0: 0,
							_1: A2(_elm_lang$core$List$range, 13, 23)
						}));
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _Leonti$elm_time_picker$CalculatedModel$outerNumbersToDisplay = F2(
	function (mode, is24Hours) {
		var _p12 = mode;
		if (_p12.ctor === 'Hours') {
			return A2(
				_elm_lang$core$List$map,
				_Leonti$elm_time_picker$CalculatedModel$toDoubleZeroString,
				{
					ctor: '::',
					_0: 12,
					_1: A2(_elm_lang$core$List$range, 1, 11)
				});
		} else {
			return A2(
				_elm_lang$core$List$map,
				_Leonti$elm_time_picker$CalculatedModel$doubleDigitFormat,
				A2(
					_elm_lang$core$List$map,
					function (x) {
						return x * 5;
					},
					A2(_elm_lang$core$List$range, 0, 11)));
		}
	});
var _Leonti$elm_time_picker$CalculatedModel$calculateModel = function (model) {
	return {
		outerNumbers: A2(_Leonti$elm_time_picker$CalculatedModel$outerNumbersToDisplay, model.mode, model.is24Hours),
		innerNumbers: A2(_Leonti$elm_time_picker$CalculatedModel$innerNumbersToDisplay, model.mode, model.is24Hours),
		pointerAngle: A4(_Leonti$elm_time_picker$CalculatedModel$toPointerAngle, model.mode, model.is24Hours, model.hoursSelected, model.minutesSelected),
		isShortPointer: (_elm_lang$core$Native_Utils.eq(model.mode, _Leonti$elm_time_picker$TimeTypes$Hours) && model.is24Hours) && _Leonti$elm_time_picker$CalculatedModel$isInnerSelected(model.hoursSelected),
		selectedNumber: A4(_Leonti$elm_time_picker$CalculatedModel$toSelectedNumber, model.mode, model.is24Hours, model.hoursSelected, model.minutesSelected),
		digitalTimeHours: A2(_Leonti$elm_time_picker$CalculatedModel$digitalTimeHoursToDisplay, model.is24Hours, model.hoursSelected),
		digitalTimeMinutes: _Leonti$elm_time_picker$CalculatedModel$doubleDigitFormat(model.minutesSelected),
		mode: model.mode,
		timePeriodSelected: _Leonti$elm_time_picker$CalculatedModel$hoursToTimePeriod(model.hoursSelected),
		timePeriodShown: !model.is24Hours
	};
};
var _Leonti$elm_time_picker$CalculatedModel$InputModel = F4(
	function (a, b, c, d) {
		return {is24Hours: a, mode: b, hoursSelected: c, minutesSelected: d};
	});
var _Leonti$elm_time_picker$CalculatedModel$CurrentTime = F2(
	function (a, b) {
		return {hoursSelected: a, minutesSelected: b};
	});
var _Leonti$elm_time_picker$CalculatedModel$CalculatedModel = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {outerNumbers: a, innerNumbers: b, pointerAngle: c, isShortPointer: d, selectedNumber: e, digitalTimeHours: f, digitalTimeMinutes: g, mode: h, timePeriodSelected: i, timePeriodShown: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};

var _Leonti$elm_time_picker$MousePosition$Offset = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _Leonti$elm_time_picker$MousePosition$eventToOffsetDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	_Leonti$elm_time_picker$MousePosition$Offset,
	A2(_elm_lang$core$Json_Decode$field, 'offsetX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'offsetY', _elm_lang$core$Json_Decode$int));
var _Leonti$elm_time_picker$MousePosition$onMouseMove = function (target) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousemove',
		A2(_elm_lang$core$Json_Decode$map, target, _Leonti$elm_time_picker$MousePosition$eventToOffsetDecoder));
};
var _Leonti$elm_time_picker$MousePosition$onMouseDown = function (target) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		A2(_elm_lang$core$Json_Decode$map, target, _Leonti$elm_time_picker$MousePosition$eventToOffsetDecoder));
};
var _Leonti$elm_time_picker$MousePosition$onMouseUp = function (target) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		A2(_elm_lang$core$Json_Decode$map, target, _Leonti$elm_time_picker$MousePosition$eventToOffsetDecoder));
};

var _Leonti$elm_time_picker$TimePicker$numberView = F5(
	function (isSelected, spanClass, number, position, mainColor) {
		var inlineStyle = {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'transform',
				_1: A2(
					_elm_lang$core$Basics_ops['++'],
					'translate(',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(position.x),
						A2(
							_elm_lang$core$Basics_ops['++'],
							'px, ',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(position.y),
								'px)'))))
			},
			_1: {ctor: '[]'}
		};
		var finalInnerStyle = isSelected ? A2(
			_elm_lang$core$Basics_ops['++'],
			inlineStyle,
			{
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'background-color', _1: mainColor},
				_1: {ctor: '[]'}
			}) : inlineStyle;
		return A2(
			_elm_lang$html$Html$span,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class(
					isSelected ? A2(_elm_lang$core$Basics_ops['++'], spanClass, ' number-selected') : spanClass),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(finalInnerStyle),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(number),
				_1: {ctor: '[]'}
			});
	});
var _Leonti$elm_time_picker$TimePicker$numberViewWrapper = F5(
	function (selectedNumber, spanClass, mainColor, number, position) {
		return A5(
			_Leonti$elm_time_picker$TimePicker$numberView,
			_elm_lang$core$Native_Utils.eq(selectedNumber, number),
			spanClass,
			number,
			position,
			mainColor);
	});
var _Leonti$elm_time_picker$TimePicker$pointerView = F3(
	function (isInner, angle, mainColor) {
		var height = isInner ? '27' : '40';
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('arrow-container'),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'height',
								_1: A2(_elm_lang$core$Basics_ops['++'], height, '%')
							},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'background-color', _1: mainColor},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'transform',
										_1: A2(
											_elm_lang$core$Basics_ops['++'],
											'rotateZ(',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_lang$core$Basics$toString(angle),
												'deg)'))
									},
									_1: {ctor: '[]'}
								}
							}
						}),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('arrow'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(
								{
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'border-color', _1: mainColor},
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			});
	});
var _Leonti$elm_time_picker$TimePicker$periodClass = F2(
	function (timePeriod, selectedTimePeriod) {
		var _p0 = timePeriod;
		if (_p0.ctor === 'AM') {
			return _elm_lang$core$Native_Utils.eq(selectedTimePeriod, _Leonti$elm_time_picker$TimeTypes$AM) ? 'am' : 'am inactive';
		} else {
			return _elm_lang$core$Native_Utils.eq(selectedTimePeriod, _Leonti$elm_time_picker$TimeTypes$AM) ? 'pm inactive' : 'pm';
		}
	});
var _Leonti$elm_time_picker$TimePicker$outerNumbers = {
	ctor: '::',
	_0: 12,
	_1: {
		ctor: '::',
		_0: 1,
		_1: {
			ctor: '::',
			_0: 2,
			_1: {
				ctor: '::',
				_0: 3,
				_1: {
					ctor: '::',
					_0: 4,
					_1: {
						ctor: '::',
						_0: 5,
						_1: {
							ctor: '::',
							_0: 6,
							_1: {
								ctor: '::',
								_0: 7,
								_1: {
									ctor: '::',
									_0: 8,
									_1: {
										ctor: '::',
										_0: 9,
										_1: {
											ctor: '::',
											_0: 10,
											_1: {
												ctor: '::',
												_0: 11,
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _Leonti$elm_time_picker$TimePicker$toDegrees = function (radians) {
	return radians * (180 / _elm_lang$core$Basics$pi);
};
var _Leonti$elm_time_picker$TimePicker$roundWithStep = F2(
	function (value, step) {
		return _elm_lang$core$Basics$round(
			value / _elm_lang$core$Basics$toFloat(step)) * step;
	});
var _Leonti$elm_time_picker$TimePicker$angleWithStep = F2(
	function (rawDegrees, step) {
		var roundedDegrees = A2(_Leonti$elm_time_picker$TimePicker$roundWithStep, rawDegrees, step);
		var angle = _elm_lang$core$Basics$floor(
			_elm_lang$core$Basics$toFloat(roundedDegrees) / _elm_lang$core$Basics$toFloat(step));
		return _elm_lang$core$Native_Utils.eq(
			angle,
			_elm_lang$core$Basics$floor(
				360 / _elm_lang$core$Basics$toFloat(step))) ? 0 : angle;
	});
var _Leonti$elm_time_picker$TimePicker$adjustedDegrees = function (value) {
	var adjustedValue = ((_elm_lang$core$Native_Utils.cmp(value, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(value, 90) < 1)) ? (90 - value) : (((_elm_lang$core$Native_Utils.cmp(value, -90) > -1) && (_elm_lang$core$Native_Utils.cmp(value, 0) < 1)) ? (90 + (0 - value)) : (((_elm_lang$core$Native_Utils.cmp(value, -190) > -1) && (_elm_lang$core$Native_Utils.cmp(value, -90) < 0)) ? (180 + ((0 - value) - 90)) : (270 + (180 - value))));
	return adjustedValue;
};
var _Leonti$elm_time_picker$TimePicker$distance = F2(
	function (position1, position2) {
		var deltaY = position1.y - position2.y;
		var deltaX = position1.x - position2.x;
		return _elm_lang$core$Basics$round(
			_elm_lang$core$Basics$sqrt((deltaX * deltaX) + (deltaY * deltaY)));
	});
var _Leonti$elm_time_picker$TimePicker$innerDistance = 90;
var _Leonti$elm_time_picker$TimePicker$center = {x: 130, y: 140};
var _Leonti$elm_time_picker$TimePicker$angleFromOffset = function (offset) {
	var y = 0 - (offset.y - _elm_lang$core$Basics$round(_Leonti$elm_time_picker$TimePicker$center.y));
	var x = offset.x - _elm_lang$core$Basics$round(_Leonti$elm_time_picker$TimePicker$center.x);
	var test = {ctor: '_Tuple2', _0: x, _1: y};
	var angleRad = A2(
		_elm_lang$core$Basics$atan2,
		_elm_lang$core$Basics$toFloat(y),
		_elm_lang$core$Basics$toFloat(x));
	return _Leonti$elm_time_picker$TimePicker$adjustedDegrees(
		_Leonti$elm_time_picker$TimePicker$toDegrees(angleRad));
};
var _Leonti$elm_time_picker$TimePicker$applySelection = F2(
	function (model, selection) {
		var selectedTime = A2(
			_Leonti$elm_time_picker$CalculatedModel$toSelectedTime,
			{
				is24Hours: model.settings.is24Hours,
				mode: model.mode,
				hoursSelected: _elm_lang$core$Date$hour(model.date),
				minutesSelected: _elm_lang$core$Date$minute(model.date)
			},
			selection);
		var updatedHoursDate = function () {
			var _p1 = A2(
				_rluiten$elm_date_extra$Date_Extra_Field$fieldToDate,
				_rluiten$elm_date_extra$Date_Extra_Field$Hour(selectedTime.hoursSelected),
				model.date);
			if (_p1.ctor === 'Just') {
				return _p1._0;
			} else {
				return model.date;
			}
		}();
		var updatedMinutesDate = function () {
			var _p2 = A2(
				_rluiten$elm_date_extra$Date_Extra_Field$fieldToDate,
				_rluiten$elm_date_extra$Date_Extra_Field$Minute(selectedTime.minutesSelected),
				updatedHoursDate);
			if (_p2.ctor === 'Just') {
				return _p2._0;
			} else {
				return updatedHoursDate;
			}
		}();
		return _elm_lang$core$Native_Utils.update(
			model,
			{date: updatedMinutesDate});
	});
var _Leonti$elm_time_picker$TimePicker$selectedTime = function (model) {
	return model.date;
};
var _Leonti$elm_time_picker$TimePicker$init = F2(
	function (settings, date) {
		return {settings: settings, mode: _Leonti$elm_time_picker$TimeTypes$Hours, date: date, isSelecting: false};
	});
var _Leonti$elm_time_picker$TimePicker$modeToStep = function (mode) {
	var _p3 = mode;
	if (_p3.ctor === 'Hours') {
		return 30;
	} else {
		return 6;
	}
};
var _Leonti$elm_time_picker$TimePicker$Position = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _Leonti$elm_time_picker$TimePicker$outerPositions = {
	ctor: '::',
	_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 0, 5),
	_1: {
		ctor: '::',
		_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 54.5, 16.6),
		_1: {
			ctor: '::',
			_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 94.4, 59.5),
			_1: {
				ctor: '::',
				_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 109, 114),
				_1: {
					ctor: '::',
					_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 94.4, 168.5),
					_1: {
						ctor: '::',
						_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 54.5, 208.4),
						_1: {
							ctor: '::',
							_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 0, 223),
							_1: {
								ctor: '::',
								_0: A2(_Leonti$elm_time_picker$TimePicker$Position, -54.5, 208.4),
								_1: {
									ctor: '::',
									_0: A2(_Leonti$elm_time_picker$TimePicker$Position, -94.4, 168.5),
									_1: {
										ctor: '::',
										_0: A2(_Leonti$elm_time_picker$TimePicker$Position, -109, 114),
										_1: {
											ctor: '::',
											_0: A2(_Leonti$elm_time_picker$TimePicker$Position, -94.4, 59.5),
											_1: {
												ctor: '::',
												_0: A2(_Leonti$elm_time_picker$TimePicker$Position, -54.5, 19.6),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _Leonti$elm_time_picker$TimePicker$innerPositions = {
	ctor: '::',
	_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 0, 40),
	_1: {
		ctor: '::',
		_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 36.9, 49.9),
		_1: {
			ctor: '::',
			_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 64, 77),
			_1: {
				ctor: '::',
				_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 74, 114),
				_1: {
					ctor: '::',
					_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 64, 151),
					_1: {
						ctor: '::',
						_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 37, 178),
						_1: {
							ctor: '::',
							_0: A2(_Leonti$elm_time_picker$TimePicker$Position, 0, 188),
							_1: {
								ctor: '::',
								_0: A2(_Leonti$elm_time_picker$TimePicker$Position, -37, 178),
								_1: {
									ctor: '::',
									_0: A2(_Leonti$elm_time_picker$TimePicker$Position, -64, 151),
									_1: {
										ctor: '::',
										_0: A2(_Leonti$elm_time_picker$TimePicker$Position, -74, 114),
										_1: {
											ctor: '::',
											_0: A2(_Leonti$elm_time_picker$TimePicker$Position, -64, 77),
											_1: {
												ctor: '::',
												_0: A2(_Leonti$elm_time_picker$TimePicker$Position, -37, 50),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _Leonti$elm_time_picker$TimePicker$offsetToSelection = F2(
	function (offset, step) {
		var dist = A2(
			_Leonti$elm_time_picker$TimePicker$distance,
			A2(
				_Leonti$elm_time_picker$TimePicker$Position,
				_elm_lang$core$Basics$toFloat(offset.x),
				_elm_lang$core$Basics$toFloat(offset.y)),
			_Leonti$elm_time_picker$TimePicker$center);
		var angle = _Leonti$elm_time_picker$TimePicker$angleFromOffset(offset);
		return {
			angle: A2(_Leonti$elm_time_picker$TimePicker$angleWithStep, angle, step),
			isInner: _elm_lang$core$Native_Utils.cmp(dist, _Leonti$elm_time_picker$TimePicker$innerDistance) < 1
		};
	});
var _Leonti$elm_time_picker$TimePicker$update = F2(
	function (msg, model) {
		var _p4 = msg;
		switch (_p4.ctor) {
			case 'Noop':
				return model;
			case 'MouseDown':
				var modelWithSelection = A2(
					_Leonti$elm_time_picker$TimePicker$applySelection,
					model,
					A2(
						_Leonti$elm_time_picker$TimePicker$offsetToSelection,
						_p4._0,
						_Leonti$elm_time_picker$TimePicker$modeToStep(model.mode)));
				return _elm_lang$core$Native_Utils.update(
					modelWithSelection,
					{isSelecting: true});
			case 'MouseMove':
				if (_elm_lang$core$Native_Utils.eq(model.isSelecting, true)) {
					var selection = A2(
						_Leonti$elm_time_picker$TimePicker$offsetToSelection,
						_p4._0,
						_Leonti$elm_time_picker$TimePicker$modeToStep(model.mode));
					return A2(_Leonti$elm_time_picker$TimePicker$applySelection, model, selection);
				} else {
					return model;
				}
			case 'MouseUp':
				var _p5 = model.mode;
				if (_p5.ctor === 'Hours') {
					return _elm_lang$core$Native_Utils.update(
						model,
						{isSelecting: false, mode: _Leonti$elm_time_picker$TimeTypes$Minutes});
				} else {
					return _elm_lang$core$Native_Utils.update(
						model,
						{isSelecting: false});
				}
			case 'ModeSwitch':
				return _elm_lang$core$Native_Utils.update(
					model,
					{mode: _p4._0});
			default:
				var hoursSelected = function () {
					var _p6 = _p4._0;
					if (_p6.ctor === 'AM') {
						return _Leonti$elm_time_picker$CalculatedModel$toAmHours(
							_elm_lang$core$Date$hour(model.date));
					} else {
						return _Leonti$elm_time_picker$CalculatedModel$toPmHours(
							_elm_lang$core$Date$hour(model.date));
					}
				}();
				var updatedDate = function () {
					var _p7 = A2(
						_rluiten$elm_date_extra$Date_Extra_Field$fieldToDate,
						_rluiten$elm_date_extra$Date_Extra_Field$Hour(hoursSelected),
						model.date);
					if (_p7.ctor === 'Just') {
						return _p7._0;
					} else {
						return model.date;
					}
				}();
				return _elm_lang$core$Native_Utils.update(
					model,
					{date: updatedDate});
		}
	});
var _Leonti$elm_time_picker$TimePicker$SelectedTime = F2(
	function (a, b) {
		return {hours: a, minutes: b};
	});
var _Leonti$elm_time_picker$TimePicker$Settings = F2(
	function (a, b) {
		return {is24Hours: a, mainColor: b};
	});
var _Leonti$elm_time_picker$TimePicker$Model = F4(
	function (a, b, c, d) {
		return {settings: a, mode: b, date: c, isSelecting: d};
	});
var _Leonti$elm_time_picker$TimePicker$MouseUp = function (a) {
	return {ctor: 'MouseUp', _0: a};
};
var _Leonti$elm_time_picker$TimePicker$MouseMove = function (a) {
	return {ctor: 'MouseMove', _0: a};
};
var _Leonti$elm_time_picker$TimePicker$MouseDown = function (a) {
	return {ctor: 'MouseDown', _0: a};
};
var _Leonti$elm_time_picker$TimePicker$clockFaceView = F2(
	function (model, cm) {
		var pointerViewToRender = A3(_Leonti$elm_time_picker$TimePicker$pointerView, cm.isShortPointer, cm.pointerAngle, model.settings.mainColor);
		var innerNumbersView = A2(
			_elm_lang$core$Maybe$withDefault,
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{ctor: '[]'},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$Maybe$map,
				function (innerNumbers) {
					return A3(
						_elm_lang$core$List$map2,
						A3(_Leonti$elm_time_picker$TimePicker$numberViewWrapper, cm.selectedNumber, 'number-inner', model.settings.mainColor),
						innerNumbers,
						_Leonti$elm_time_picker$TimePicker$innerPositions);
				},
				cm.innerNumbers));
		var outerNumbersView = A3(
			_elm_lang$core$List$map2,
			A3(_Leonti$elm_time_picker$TimePicker$numberViewWrapper, cm.selectedNumber, 'number-outer', model.settings.mainColor),
			cm.outerNumbers,
			_Leonti$elm_time_picker$TimePicker$outerPositions);
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('time-picker-clock-face'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('clock-face-background'),
						_1: {ctor: '[]'}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('numbers-container'),
							_1: {
								ctor: '::',
								_0: _Leonti$elm_time_picker$MousePosition$onMouseMove(_Leonti$elm_time_picker$TimePicker$MouseMove),
								_1: {
									ctor: '::',
									_0: _Leonti$elm_time_picker$MousePosition$onMouseDown(_Leonti$elm_time_picker$TimePicker$MouseDown),
									_1: {
										ctor: '::',
										_0: _Leonti$elm_time_picker$MousePosition$onMouseUp(_Leonti$elm_time_picker$TimePicker$MouseUp),
										_1: {ctor: '[]'}
									}
								}
							}
						},
						A2(
							_elm_lang$core$Basics_ops['++'],
							{ctor: '::', _0: pointerViewToRender, _1: outerNumbersView},
							A2(
								_elm_lang$core$Basics_ops['++'],
								innerNumbersView,
								{
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$div,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('filler'),
											_1: {ctor: '[]'}
										},
										{ctor: '[]'}),
									_1: {ctor: '[]'}
								}))),
					_1: {ctor: '[]'}
				}
			});
	});
var _Leonti$elm_time_picker$TimePicker$TimePeriodSwitch = function (a) {
	return {ctor: 'TimePeriodSwitch', _0: a};
};
var _Leonti$elm_time_picker$TimePicker$ModeSwitch = function (a) {
	return {ctor: 'ModeSwitch', _0: a};
};
var _Leonti$elm_time_picker$TimePicker$digitalNumberDisplay = F3(
	function (isActive, value, otherMode) {
		var _p8 = isActive;
		if (_p8 === true) {
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('number'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onClick(
							_Leonti$elm_time_picker$TimePicker$ModeSwitch(otherMode)),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(value),
					_1: {ctor: '[]'}
				});
		} else {
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('number'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'opacity', _1: '0.7'},
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onClick(
								_Leonti$elm_time_picker$TimePicker$ModeSwitch(otherMode)),
							_1: {ctor: '[]'}
						}
					}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(value),
					_1: {ctor: '[]'}
				});
		}
	});
var _Leonti$elm_time_picker$TimePicker$digitalNumbersDisplay = F3(
	function (mode, hours, minutes) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('time-display-numbers'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A3(
					_Leonti$elm_time_picker$TimePicker$digitalNumberDisplay,
					_elm_lang$core$Native_Utils.eq(mode, _Leonti$elm_time_picker$TimeTypes$Hours),
					hours,
					_Leonti$elm_time_picker$TimeTypes$Hours),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(':'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A3(
							_Leonti$elm_time_picker$TimePicker$digitalNumberDisplay,
							_elm_lang$core$Native_Utils.eq(mode, _Leonti$elm_time_picker$TimeTypes$Minutes),
							minutes,
							_Leonti$elm_time_picker$TimeTypes$Minutes),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _Leonti$elm_time_picker$TimePicker$timeDisplay24h = F3(
	function (mode, hours, minutes) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('time-display-numbers-container'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('side-filler'),
						_1: {ctor: '[]'}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A3(_Leonti$elm_time_picker$TimePicker$digitalNumbersDisplay, mode, hours, minutes),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('side-filler'),
								_1: {ctor: '[]'}
							},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _Leonti$elm_time_picker$TimePicker$timeDisplay12h = F4(
	function (mode, timePeriod, hours, minutes) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('time-display-numbers-container'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('side-filler'),
						_1: {ctor: '[]'}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A3(_Leonti$elm_time_picker$TimePicker$digitalNumbersDisplay, mode, hours, minutes),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('time-periods'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$div,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class(
											A2(_Leonti$elm_time_picker$TimePicker$periodClass, _Leonti$elm_time_picker$TimeTypes$PM, timePeriod)),
										_1: {
											ctor: '::',
											_0: _elm_lang$html$Html_Events$onClick(
												_Leonti$elm_time_picker$TimePicker$TimePeriodSwitch(_Leonti$elm_time_picker$TimeTypes$PM)),
											_1: {ctor: '[]'}
										}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('PM'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$div,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class(
												A2(_Leonti$elm_time_picker$TimePicker$periodClass, _Leonti$elm_time_picker$TimeTypes$AM, timePeriod)),
											_1: {
												ctor: '::',
												_0: _elm_lang$html$Html_Events$onClick(
													_Leonti$elm_time_picker$TimePicker$TimePeriodSwitch(_Leonti$elm_time_picker$TimeTypes$AM)),
												_1: {ctor: '[]'}
											}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text('AM'),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}
							}),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _Leonti$elm_time_picker$TimePicker$digitalTimeView = function (cm) {
	var _p9 = cm.timePeriodShown;
	if (_p9 === true) {
		return A4(_Leonti$elm_time_picker$TimePicker$timeDisplay12h, cm.mode, cm.timePeriodSelected, cm.digitalTimeHours, cm.digitalTimeMinutes);
	} else {
		return A3(_Leonti$elm_time_picker$TimePicker$timeDisplay24h, cm.mode, cm.digitalTimeHours, cm.digitalTimeMinutes);
	}
};
var _Leonti$elm_time_picker$TimePicker$view = function (model) {
	var calculatedModel = _Leonti$elm_time_picker$CalculatedModel$calculateModel(
		{
			is24Hours: model.settings.is24Hours,
			mode: model.mode,
			hoursSelected: _elm_lang$core$Date$hour(model.date),
			minutesSelected: _elm_lang$core$Date$minute(model.date)
		});
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('time-picker'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('time-display'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'background-color', _1: model.settings.mainColor},
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: _Leonti$elm_time_picker$TimePicker$digitalTimeView(calculatedModel),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(_Leonti$elm_time_picker$TimePicker$clockFaceView, model, calculatedModel),
				_1: {ctor: '[]'}
			}
		});
};
var _Leonti$elm_time_picker$TimePicker$Noop = {ctor: 'Noop'};

var _debois$elm_dom$DOM$className = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'className',
		_1: {ctor: '[]'}
	},
	_elm_lang$core$Json_Decode$string);
var _debois$elm_dom$DOM$scrollTop = A2(_elm_lang$core$Json_Decode$field, 'scrollTop', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$scrollLeft = A2(_elm_lang$core$Json_Decode$field, 'scrollLeft', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetTop = A2(_elm_lang$core$Json_Decode$field, 'offsetTop', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetLeft = A2(_elm_lang$core$Json_Decode$field, 'offsetLeft', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetHeight = A2(_elm_lang$core$Json_Decode$field, 'offsetHeight', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetWidth = A2(_elm_lang$core$Json_Decode$field, 'offsetWidth', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$childNodes = function (decoder) {
	var loop = F2(
		function (idx, xs) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (_p0) {
					return A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Json_Decode$succeed(xs),
						A2(
							_elm_lang$core$Maybe$map,
							function (x) {
								return A2(
									loop,
									idx + 1,
									{ctor: '::', _0: x, _1: xs});
							},
							_p0));
				},
				_elm_lang$core$Json_Decode$maybe(
					A2(
						_elm_lang$core$Json_Decode$field,
						_elm_lang$core$Basics$toString(idx),
						decoder)));
		});
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$List$reverse,
		A2(
			_elm_lang$core$Json_Decode$field,
			'childNodes',
			A2(
				loop,
				0,
				{ctor: '[]'})));
};
var _debois$elm_dom$DOM$childNode = function (idx) {
	return _elm_lang$core$Json_Decode$at(
		{
			ctor: '::',
			_0: 'childNodes',
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(idx),
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_dom$DOM$parentElement = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'parentElement', decoder);
};
var _debois$elm_dom$DOM$previousSibling = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'previousSibling', decoder);
};
var _debois$elm_dom$DOM$nextSibling = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'nextSibling', decoder);
};
var _debois$elm_dom$DOM$offsetParent = F2(
	function (x, decoder) {
		return _elm_lang$core$Json_Decode$oneOf(
			{
				ctor: '::',
				_0: A2(
					_elm_lang$core$Json_Decode$field,
					'offsetParent',
					_elm_lang$core$Json_Decode$null(x)),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$Json_Decode$field, 'offsetParent', decoder),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_dom$DOM$position = F2(
	function (x, y) {
		return A2(
			_elm_lang$core$Json_Decode$andThen,
			function (_p1) {
				var _p2 = _p1;
				var _p4 = _p2._1;
				var _p3 = _p2._0;
				return A2(
					_debois$elm_dom$DOM$offsetParent,
					{ctor: '_Tuple2', _0: _p3, _1: _p4},
					A2(_debois$elm_dom$DOM$position, _p3, _p4));
			},
			A5(
				_elm_lang$core$Json_Decode$map4,
				F4(
					function (scrollLeft, scrollTop, offsetLeft, offsetTop) {
						return {ctor: '_Tuple2', _0: (x + offsetLeft) - scrollLeft, _1: (y + offsetTop) - scrollTop};
					}),
				_debois$elm_dom$DOM$scrollLeft,
				_debois$elm_dom$DOM$scrollTop,
				_debois$elm_dom$DOM$offsetLeft,
				_debois$elm_dom$DOM$offsetTop));
	});
var _debois$elm_dom$DOM$boundingClientRect = A4(
	_elm_lang$core$Json_Decode$map3,
	F3(
		function (_p5, width, height) {
			var _p6 = _p5;
			return {top: _p6._1, left: _p6._0, width: width, height: height};
		}),
	A2(_debois$elm_dom$DOM$position, 0, 0),
	_debois$elm_dom$DOM$offsetWidth,
	_debois$elm_dom$DOM$offsetHeight);
var _debois$elm_dom$DOM$target = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'target', decoder);
};
var _debois$elm_dom$DOM$Rectangle = F4(
	function (a, b, c, d) {
		return {top: a, left: b, width: c, height: d};
	});

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _debois$elm_mdl$Material_Helpers$noAttr = A2(_elm_lang$html$Html_Attributes$attribute, 'data-elm-mdl-noop', '');
var _debois$elm_mdl$Material_Helpers$aria = F2(
	function (name, value) {
		return value ? A2(
			_elm_lang$html$Html_Attributes$attribute,
			A2(_elm_lang$core$Basics_ops['++'], 'aria-', name),
			'true') : _debois$elm_mdl$Material_Helpers$noAttr;
	});
var _debois$elm_mdl$Material_Helpers$delay = F2(
	function (t, x) {
		return A2(
			_elm_lang$core$Task$perform,
			_elm_lang$core$Basics$always(x),
			_elm_lang$core$Process$sleep(t));
	});
var _debois$elm_mdl$Material_Helpers$cssTransitionStep = function (x) {
	return A2(_debois$elm_mdl$Material_Helpers$delay, 50, x);
};
var _debois$elm_mdl$Material_Helpers$cmd = function (msg) {
	return A2(
		_elm_lang$core$Task$perform,
		_elm_lang$core$Basics$always(msg),
		_elm_lang$core$Task$succeed(msg));
};
var _debois$elm_mdl$Material_Helpers$lift = F6(
	function (get, set, fwd, update, action, model) {
		var _p0 = A2(
			update,
			action,
			get(model));
		var submodel_ = _p0._0;
		var e = _p0._1;
		return {
			ctor: '_Tuple2',
			_0: A2(set, model, submodel_),
			_1: A2(_elm_lang$core$Platform_Cmd$map, fwd, e)
		};
	});
var _debois$elm_mdl$Material_Helpers$lift_ = F5(
	function (get, set, update, action, model) {
		return {
			ctor: '_Tuple2',
			_0: A2(
				set,
				model,
				A2(
					update,
					action,
					get(model))),
			_1: _elm_lang$core$Platform_Cmd$none
		};
	});
var _debois$elm_mdl$Material_Helpers$map2nd = F2(
	function (f, _p1) {
		var _p2 = _p1;
		return {
			ctor: '_Tuple2',
			_0: _p2._0,
			_1: f(_p2._1)
		};
	});
var _debois$elm_mdl$Material_Helpers$map1st = F2(
	function (f, _p3) {
		var _p4 = _p3;
		return {
			ctor: '_Tuple2',
			_0: f(_p4._0),
			_1: _p4._1
		};
	});
var _debois$elm_mdl$Material_Helpers$blurOn = function (evt) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		A2(_elm_lang$core$Basics_ops['++'], 'on', evt),
		'this.blur()');
};
var _debois$elm_mdl$Material_Helpers$effect = F2(
	function (e, x) {
		return {ctor: '_Tuple2', _0: x, _1: e};
	});
var _debois$elm_mdl$Material_Helpers$pure = _debois$elm_mdl$Material_Helpers$effect(_elm_lang$core$Platform_Cmd$none);
var _debois$elm_mdl$Material_Helpers$filter = F3(
	function (elem, attr, html) {
		return A2(
			elem,
			attr,
			A2(
				_elm_lang$core$List$filterMap,
				function (x) {
					return x;
				},
				html));
	});

var _debois$elm_mdl$Material_Component$subs = F5(
	function (ctor, get, subscriptions, lift, model) {
		return _elm_lang$core$Platform_Sub$batch(
			A3(
				_elm_lang$core$Dict$foldl,
				F3(
					function (idx, model, ss) {
						return {
							ctor: '::',
							_0: A2(
								_elm_lang$core$Platform_Sub$map,
								function (_p0) {
									return lift(
										A2(ctor, idx, _p0));
								},
								subscriptions(model)),
							_1: ss
						};
					}),
				{ctor: '[]'},
				get(model)));
	});
var _debois$elm_mdl$Material_Component$generalise = F4(
	function (update, lift, msg, model) {
		return A2(
			_debois$elm_mdl$Material_Helpers$map2nd,
			_elm_lang$core$Platform_Cmd$map(lift),
			A2(
				_debois$elm_mdl$Material_Helpers$map1st,
				_elm_lang$core$Maybe$Just,
				A2(update, msg, model)));
	});
var _debois$elm_mdl$Material_Component$react = F8(
	function (get, set, ctor, update, lift, msg, idx, store) {
		return A2(
			_debois$elm_mdl$Material_Helpers$map1st,
			_elm_lang$core$Maybe$map(
				A2(set, idx, store)),
			A3(
				update,
				function (_p1) {
					return lift(
						A2(ctor, idx, _p1));
				},
				msg,
				A2(get, idx, store)));
	});
var _debois$elm_mdl$Material_Component$react1 = F7(
	function (get, set, ctor, update, lift, msg, store) {
		return A2(
			_debois$elm_mdl$Material_Helpers$map1st,
			_elm_lang$core$Maybe$map(
				set(store)),
			A3(
				update,
				function (_p2) {
					return lift(
						ctor(_p2));
				},
				msg,
				get(store)));
	});
var _debois$elm_mdl$Material_Component$render = F6(
	function (get_model, view, ctor, lift, idx, store) {
		return A2(
			view,
			function (_p3) {
				return lift(
					A2(ctor, idx, _p3));
			},
			A2(get_model, idx, store));
	});
var _debois$elm_mdl$Material_Component$render1 = F5(
	function (get_model, view, ctor, lift, store) {
		return A2(
			view,
			function (_p4) {
				return lift(
					ctor(_p4));
			},
			get_model(store));
	});
var _debois$elm_mdl$Material_Component$indexed = F3(
	function (get_model, set_model, model0) {
		var set_ = F3(
			function (idx, store, model) {
				return A2(
					set_model,
					A3(
						_elm_lang$core$Dict$insert,
						idx,
						model,
						get_model(store)),
					store);
			});
		var get_ = F2(
			function (idx, store) {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					model0,
					A2(
						_elm_lang$core$Dict$get,
						idx,
						get_model(store)));
			});
		return {ctor: '_Tuple2', _0: get_, _1: set_};
	});
var _debois$elm_mdl$Material_Component$Dispatch = function (a) {
	return {ctor: 'Dispatch', _0: a};
};
var _debois$elm_mdl$Material_Component$TabsMsg = F2(
	function (a, b) {
		return {ctor: 'TabsMsg', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Component$TooltipMsg = F2(
	function (a, b) {
		return {ctor: 'TooltipMsg', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Component$TogglesMsg = F2(
	function (a, b) {
		return {ctor: 'TogglesMsg', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Component$LayoutMsg = function (a) {
	return {ctor: 'LayoutMsg', _0: a};
};
var _debois$elm_mdl$Material_Component$MenuMsg = F2(
	function (a, b) {
		return {ctor: 'MenuMsg', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Component$TextfieldMsg = F2(
	function (a, b) {
		return {ctor: 'TextfieldMsg', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Component$ButtonMsg = F2(
	function (a, b) {
		return {ctor: 'ButtonMsg', _0: a, _1: b};
	});

var _debois$elm_mdl$Material_Dispatch$split = F4(
	function (k0, same, differ, xs) {
		split:
		while (true) {
			var _p0 = xs;
			if (_p0.ctor === '[]') {
				return {ctor: '_Tuple2', _0: same, _1: differ};
			} else {
				var _p1 = _p0._1;
				if (_elm_lang$core$Native_Utils.eq(_p0._0._0, k0)) {
					var _v1 = k0,
						_v2 = {ctor: '::', _0: _p0._0._1, _1: same},
						_v3 = differ,
						_v4 = _p1;
					k0 = _v1;
					same = _v2;
					differ = _v3;
					xs = _v4;
					continue split;
				} else {
					var _v5 = k0,
						_v6 = same,
						_v7 = {ctor: '::', _0: _p0._0, _1: differ},
						_v8 = _p1;
					k0 = _v5;
					same = _v6;
					differ = _v7;
					xs = _v8;
					continue split;
				}
			}
		}
	});
var _debois$elm_mdl$Material_Dispatch$group_ = F2(
	function (acc, items) {
		group_:
		while (true) {
			var _p2 = items;
			if (_p2.ctor === '[]') {
				return acc;
			} else {
				if (_p2._1.ctor === '[]') {
					return {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: _p2._0._0,
							_1: {
								ctor: '::',
								_0: _p2._0._1,
								_1: {ctor: '[]'}
							}
						},
						_1: acc
					};
				} else {
					if ((_p2._1._0.ctor === '_Tuple2') && (_p2._1._1.ctor === '[]')) {
						var _p6 = _p2._1._0._1;
						var _p5 = _p2._0._1;
						var _p4 = _p2._1._0._0;
						var _p3 = _p2._0._0;
						return _elm_lang$core$Native_Utils.eq(_p3, _p4) ? {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: _p3,
								_1: {
									ctor: '::',
									_0: _p6,
									_1: {
										ctor: '::',
										_0: _p5,
										_1: {ctor: '[]'}
									}
								}
							},
							_1: acc
						} : {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: _p4,
								_1: {
									ctor: '::',
									_0: _p6,
									_1: {ctor: '[]'}
								}
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: _p3,
									_1: {
										ctor: '::',
										_0: _p5,
										_1: {ctor: '[]'}
									}
								},
								_1: acc
							}
						};
					} else {
						var _p8 = _p2._0._0;
						var _p7 = A4(
							_debois$elm_mdl$Material_Dispatch$split,
							_p8,
							{
								ctor: '::',
								_0: _p2._0._1,
								_1: {ctor: '[]'}
							},
							{ctor: '[]'},
							_p2._1);
						var same = _p7._0;
						var different = _p7._1;
						var _v10 = {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: _p8, _1: same},
							_1: acc
						},
							_v11 = different;
						acc = _v10;
						items = _v11;
						continue group_;
					}
				}
			}
		}
	});
var _debois$elm_mdl$Material_Dispatch$group = _debois$elm_mdl$Material_Dispatch$group_(
	{ctor: '[]'});
var _debois$elm_mdl$Material_Dispatch$onSingle = function (_p9) {
	var _p10 = _p9;
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		_p10._0,
		A2(_elm_lang$core$Maybe$withDefault, _elm_lang$html$Html_Events$defaultOptions, _p10._1._1),
		_p10._1._0);
};
var _debois$elm_mdl$Material_Dispatch$pickOptions = function (decoders) {
	pickOptions:
	while (true) {
		var _p11 = decoders;
		if (_p11.ctor === '::') {
			if ((_p11._0.ctor === '_Tuple2') && (_p11._0._1.ctor === 'Just')) {
				return _p11._0._1._0;
			} else {
				var _v14 = _p11._1;
				decoders = _v14;
				continue pickOptions;
			}
		} else {
			return _elm_lang$html$Html_Events$defaultOptions;
		}
	}
};
var _debois$elm_mdl$Material_Dispatch$flatten = function (decoders) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		function (value) {
			return A2(
				_elm_lang$core$List$filterMap,
				function (decoder) {
					return _elm_lang$core$Result$toMaybe(
						A2(_elm_lang$core$Json_Decode$decodeValue, decoder, value));
				},
				decoders);
		},
		_elm_lang$core$Json_Decode$value);
};
var _debois$elm_mdl$Material_Dispatch$onWithOptions = F4(
	function (event, lift, options, decoders) {
		return A3(
			_elm_lang$html$Html_Events$onWithOptions,
			event,
			options,
			A2(
				_elm_lang$core$Json_Decode$map,
				lift,
				_debois$elm_mdl$Material_Dispatch$flatten(decoders)));
	});
var _debois$elm_mdl$Material_Dispatch$on = F2(
	function (event, lift) {
		return A3(_debois$elm_mdl$Material_Dispatch$onWithOptions, event, lift, _elm_lang$html$Html_Events$defaultOptions);
	});
var _debois$elm_mdl$Material_Dispatch$onMany = F2(
	function (lift, decoders) {
		var _p12 = decoders;
		if ((_p12._1.ctor === '::') && (_p12._1._1.ctor === '[]')) {
			return _debois$elm_mdl$Material_Dispatch$onSingle(
				{ctor: '_Tuple2', _0: _p12._0, _1: _p12._1._0});
		} else {
			var _p13 = _p12._1;
			return A3(
				_elm_lang$html$Html_Events$onWithOptions,
				_p12._0,
				_debois$elm_mdl$Material_Dispatch$pickOptions(_p13),
				lift(
					_debois$elm_mdl$Material_Dispatch$flatten(
						A2(_elm_lang$core$List$map, _elm_lang$core$Tuple$first, _p13))));
		}
	});
var _debois$elm_mdl$Material_Dispatch$map2nd = F2(
	function (f, _p14) {
		var _p15 = _p14;
		return {
			ctor: '_Tuple2',
			_0: _p15._0,
			_1: f(_p15._1)
		};
	});
var _debois$elm_mdl$Material_Dispatch$update1 = F3(
	function (update, cmd, _p16) {
		var _p17 = _p16;
		return A2(
			_debois$elm_mdl$Material_Dispatch$map2nd,
			A2(
				_elm_lang$core$Basics$flip,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				_p17._1),
			A2(update, cmd, _p17._0));
	});
var _debois$elm_mdl$Material_Dispatch$update = F3(
	function (update, msg, model) {
		return A2(
			_debois$elm_mdl$Material_Dispatch$map2nd,
			_elm_lang$core$Platform_Cmd$batch,
			A3(
				_elm_lang$core$List$foldl,
				_debois$elm_mdl$Material_Dispatch$update1(update),
				{
					ctor: '_Tuple2',
					_0: model,
					_1: {ctor: '[]'}
				},
				msg));
	});
var _debois$elm_mdl$Material_Dispatch$cmd = function (msg) {
	return A2(
		_elm_lang$core$Task$perform,
		_elm_lang$core$Basics$always(msg),
		_elm_lang$core$Task$succeed(msg));
};
var _debois$elm_mdl$Material_Dispatch$forward = function (messages) {
	return _elm_lang$core$Platform_Cmd$batch(
		A2(_elm_lang$core$List$map, _debois$elm_mdl$Material_Dispatch$cmd, messages));
};
var _debois$elm_mdl$Material_Dispatch$toAttributes = function (_p18) {
	var _p19 = _p18;
	var _p21 = _p19._0;
	var _p20 = _p21.lift;
	if (_p20.ctor === 'Just') {
		return A2(
			_elm_lang$core$List$map,
			_debois$elm_mdl$Material_Dispatch$onMany(_p20._0),
			_debois$elm_mdl$Material_Dispatch$group(_p21.decoders));
	} else {
		return A2(_elm_lang$core$List$map, _debois$elm_mdl$Material_Dispatch$onSingle, _p21.decoders);
	}
};
var _debois$elm_mdl$Material_Dispatch$getDecoder = function (_p22) {
	var _p23 = _p22;
	return _p23._0.lift;
};
var _debois$elm_mdl$Material_Dispatch$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _debois$elm_mdl$Material_Dispatch$defaultConfig = _debois$elm_mdl$Material_Dispatch$Config(
	{
		decoders: {ctor: '[]'},
		lift: _elm_lang$core$Maybe$Nothing
	});
var _debois$elm_mdl$Material_Dispatch$setDecoder = F2(
	function (f, _p24) {
		var _p25 = _p24;
		return _debois$elm_mdl$Material_Dispatch$Config(
			_elm_lang$core$Native_Utils.update(
				_p25._0,
				{
					lift: _elm_lang$core$Maybe$Just(f)
				}));
	});
var _debois$elm_mdl$Material_Dispatch$setMsg = function (_p26) {
	return _debois$elm_mdl$Material_Dispatch$setDecoder(
		_elm_lang$core$Json_Decode$map(_p26));
};
var _debois$elm_mdl$Material_Dispatch$add = F4(
	function (event, options, decoder, _p27) {
		var _p28 = _p27;
		var _p29 = _p28._0;
		return _debois$elm_mdl$Material_Dispatch$Config(
			_elm_lang$core$Native_Utils.update(
				_p29,
				{
					decoders: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: event,
							_1: {ctor: '_Tuple2', _0: decoder, _1: options}
						},
						_1: _p29.decoders
					}
				}));
	});
var _debois$elm_mdl$Material_Dispatch$clear = function (_p30) {
	var _p31 = _p30;
	return _debois$elm_mdl$Material_Dispatch$Config(
		_elm_lang$core$Native_Utils.update(
			_p31._0,
			{
				decoders: {ctor: '[]'}
			}));
};

var _debois$elm_mdl$Material_Options_Internal$addAttributes = F2(
	function (summary, attrs) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			summary.attrs,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(summary.css),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class(
							A2(_elm_lang$core$String$join, ' ', summary.classes)),
						_1: {ctor: '[]'}
					}
				},
				A2(
					_elm_lang$core$Basics_ops['++'],
					attrs,
					A2(
						_elm_lang$core$Basics_ops['++'],
						summary.internal,
						_debois$elm_mdl$Material_Dispatch$toAttributes(summary.dispatch)))));
	});
var _debois$elm_mdl$Material_Options_Internal$collect1_ = F2(
	function (options, acc) {
		var _p0 = options;
		switch (_p0.ctor) {
			case 'Class':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						classes: {ctor: '::', _0: _p0._0, _1: acc.classes}
					});
			case 'CSS':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						css: {ctor: '::', _0: _p0._0, _1: acc.css}
					});
			case 'Attribute':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						attrs: {ctor: '::', _0: _p0._0, _1: acc.attrs}
					});
			case 'Internal':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						internal: {ctor: '::', _0: _p0._0, _1: acc.internal}
					});
			case 'Listener':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						dispatch: A4(_debois$elm_mdl$Material_Dispatch$add, _p0._0, _p0._1, _p0._2, acc.dispatch)
					});
			case 'Many':
				return A3(_elm_lang$core$List$foldl, _debois$elm_mdl$Material_Options_Internal$collect1_, acc, _p0._0);
			case 'Lift':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						dispatch: A2(_debois$elm_mdl$Material_Dispatch$setDecoder, _p0._0, acc.dispatch)
					});
			case 'Set':
				return acc;
			default:
				return acc;
		}
	});
var _debois$elm_mdl$Material_Options_Internal$collect1 = F2(
	function (option, acc) {
		var _p1 = option;
		switch (_p1.ctor) {
			case 'Class':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						classes: {ctor: '::', _0: _p1._0, _1: acc.classes}
					});
			case 'CSS':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						css: {ctor: '::', _0: _p1._0, _1: acc.css}
					});
			case 'Attribute':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						attrs: {ctor: '::', _0: _p1._0, _1: acc.attrs}
					});
			case 'Internal':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						internal: {ctor: '::', _0: _p1._0, _1: acc.internal}
					});
			case 'Many':
				return A3(_elm_lang$core$List$foldl, _debois$elm_mdl$Material_Options_Internal$collect1, acc, _p1._0);
			case 'Set':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						config: _p1._0(acc.config)
					});
			case 'Listener':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						dispatch: A4(_debois$elm_mdl$Material_Dispatch$add, _p1._0, _p1._1, _p1._2, acc.dispatch)
					});
			case 'Lift':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						dispatch: A2(_debois$elm_mdl$Material_Dispatch$setDecoder, _p1._0, acc.dispatch)
					});
			default:
				return acc;
		}
	});
var _debois$elm_mdl$Material_Options_Internal$recollect = _elm_lang$core$List$foldl(_debois$elm_mdl$Material_Options_Internal$collect1);
var _debois$elm_mdl$Material_Options_Internal$apply = F4(
	function (summary, ctor, options, attrs) {
		return ctor(
			A2(
				_debois$elm_mdl$Material_Options_Internal$addAttributes,
				A2(_debois$elm_mdl$Material_Options_Internal$recollect, summary, options),
				attrs));
	});
var _debois$elm_mdl$Material_Options_Internal$Summary = F6(
	function (a, b, c, d, e, f) {
		return {classes: a, css: b, attrs: c, internal: d, dispatch: e, config: f};
	});
var _debois$elm_mdl$Material_Options_Internal$collect = function (_p2) {
	return _debois$elm_mdl$Material_Options_Internal$recollect(
		A6(
			_debois$elm_mdl$Material_Options_Internal$Summary,
			{ctor: '[]'},
			{ctor: '[]'},
			{ctor: '[]'},
			{ctor: '[]'},
			_debois$elm_mdl$Material_Dispatch$defaultConfig,
			_p2));
};
var _debois$elm_mdl$Material_Options_Internal$collect_ = A2(
	_elm_lang$core$List$foldl,
	_debois$elm_mdl$Material_Options_Internal$collect1_,
	A6(
		_debois$elm_mdl$Material_Options_Internal$Summary,
		{ctor: '[]'},
		{ctor: '[]'},
		{ctor: '[]'},
		{ctor: '[]'},
		_debois$elm_mdl$Material_Dispatch$defaultConfig,
		{ctor: '_Tuple0'}));
var _debois$elm_mdl$Material_Options_Internal$None = {ctor: 'None'};
var _debois$elm_mdl$Material_Options_Internal$Lift = function (a) {
	return {ctor: 'Lift', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$dispatch = function (lift) {
	return _debois$elm_mdl$Material_Options_Internal$Lift(
		function (_p3) {
			return A2(
				_elm_lang$core$Json_Decode$map,
				lift,
				A2(_elm_lang$core$Json_Decode$map, _debois$elm_mdl$Material_Component$Dispatch, _p3));
		});
};
var _debois$elm_mdl$Material_Options_Internal$inject = F5(
	function (view, lift, a, b, c) {
		return A3(
			view,
			a,
			b,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options_Internal$dispatch(lift),
				_1: c
			});
	});
var _debois$elm_mdl$Material_Options_Internal$Listener = F3(
	function (a, b, c) {
		return {ctor: 'Listener', _0: a, _1: b, _2: c};
	});
var _debois$elm_mdl$Material_Options_Internal$on1 = F3(
	function (event, lift, m) {
		return A3(
			_debois$elm_mdl$Material_Options_Internal$Listener,
			event,
			_elm_lang$core$Maybe$Nothing,
			A2(
				_elm_lang$core$Json_Decode$map,
				lift,
				_elm_lang$core$Json_Decode$succeed(m)));
	});
var _debois$elm_mdl$Material_Options_Internal$Set = function (a) {
	return {ctor: 'Set', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$option = _debois$elm_mdl$Material_Options_Internal$Set;
var _debois$elm_mdl$Material_Options_Internal$Many = function (a) {
	return {ctor: 'Many', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$applyContainer = F3(
	function (summary, ctor, options) {
		return A4(
			_debois$elm_mdl$Material_Options_Internal$apply,
			_elm_lang$core$Native_Utils.update(
				summary,
				{
					dispatch: _debois$elm_mdl$Material_Dispatch$clear(summary.dispatch),
					attrs: {ctor: '[]'},
					internal: {ctor: '[]'},
					config: {ctor: '_Tuple0'}
				}),
			ctor,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options_Internal$Many(summary.config.container),
				_1: options
			},
			{ctor: '[]'});
	});
var _debois$elm_mdl$Material_Options_Internal$applyInput = F3(
	function (summary, ctor, options) {
		return A4(
			_debois$elm_mdl$Material_Options_Internal$apply,
			_elm_lang$core$Native_Utils.update(
				summary,
				{
					classes: {ctor: '[]'},
					css: {ctor: '[]'},
					config: {ctor: '_Tuple0'}
				}),
			ctor,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options_Internal$Many(summary.config.input),
				_1: options
			},
			{ctor: '[]'});
	});
var _debois$elm_mdl$Material_Options_Internal$input = function (_p4) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (style, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						input: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options_Internal$Many(style),
							_1: config.input
						}
					});
			})(_p4));
};
var _debois$elm_mdl$Material_Options_Internal$container = function (_p5) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (style, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						container: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options_Internal$Many(style),
							_1: config.container
						}
					});
			})(_p5));
};
var _debois$elm_mdl$Material_Options_Internal$Internal = function (a) {
	return {ctor: 'Internal', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$attribute = _debois$elm_mdl$Material_Options_Internal$Internal;
var _debois$elm_mdl$Material_Options_Internal$Attribute = function (a) {
	return {ctor: 'Attribute', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$CSS = function (a) {
	return {ctor: 'CSS', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$Class = function (a) {
	return {ctor: 'Class', _0: a};
};

var _debois$elm_mdl$Material_Options$dispatch = function (_p0) {
	return _debois$elm_mdl$Material_Options_Internal$Lift(
		_elm_lang$core$Json_Decode$map(_p0));
};
var _debois$elm_mdl$Material_Options$onWithOptions = F2(
	function (evt, options) {
		return A2(
			_debois$elm_mdl$Material_Options_Internal$Listener,
			evt,
			_elm_lang$core$Maybe$Just(options));
	});
var _debois$elm_mdl$Material_Options$on = function (event) {
	return A2(_debois$elm_mdl$Material_Options_Internal$Listener, event, _elm_lang$core$Maybe$Nothing);
};
var _debois$elm_mdl$Material_Options$on1 = F2(
	function (event, m) {
		return A2(
			_debois$elm_mdl$Material_Options$on,
			event,
			_elm_lang$core$Json_Decode$succeed(m));
	});
var _debois$elm_mdl$Material_Options$onToggle = _debois$elm_mdl$Material_Options$on1('change');
var _debois$elm_mdl$Material_Options$onClick = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onDoubleClick = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseDown = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseUp = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseEnter = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseLeave = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseOver = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseOut = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onCheck = function (_p1) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'change',
		A3(_elm_lang$core$Basics$flip, _elm_lang$core$Json_Decode$map, _elm_lang$html$Html_Events$targetChecked, _p1));
};
var _debois$elm_mdl$Material_Options$onBlur = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onFocus = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onInput = function (f) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, f, _elm_lang$html$Html_Events$targetValue));
};
var _debois$elm_mdl$Material_Options$container = _debois$elm_mdl$Material_Options_Internal$container;
var _debois$elm_mdl$Material_Options$input = _debois$elm_mdl$Material_Options_Internal$input;
var _debois$elm_mdl$Material_Options$id = function (_p2) {
	return _debois$elm_mdl$Material_Options_Internal$Attribute(
		_elm_lang$html$Html_Attributes$id(_p2));
};
var _debois$elm_mdl$Material_Options$attr = _debois$elm_mdl$Material_Options_Internal$Attribute;
var _debois$elm_mdl$Material_Options$attribute = _debois$elm_mdl$Material_Options_Internal$Attribute;
var _debois$elm_mdl$Material_Options$stylesheet = function (css) {
	return A3(
		_elm_lang$html$Html$node,
		'style',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(css),
			_1: {ctor: '[]'}
		});
};
var _debois$elm_mdl$Material_Options$data = F2(
	function (key, val) {
		return _debois$elm_mdl$Material_Options_Internal$Attribute(
			A2(
				_elm_lang$html$Html_Attributes$attribute,
				A2(_elm_lang$core$Basics_ops['++'], 'data-', key),
				val));
	});
var _debois$elm_mdl$Material_Options$nop = _debois$elm_mdl$Material_Options_Internal$None;
var _debois$elm_mdl$Material_Options$when = F2(
	function (guard, prop) {
		return guard ? prop : _debois$elm_mdl$Material_Options$nop;
	});
var _debois$elm_mdl$Material_Options$maybe = function (prop) {
	return A2(_elm_lang$core$Maybe$withDefault, _debois$elm_mdl$Material_Options$nop, prop);
};
var _debois$elm_mdl$Material_Options$many = _debois$elm_mdl$Material_Options_Internal$Many;
var _debois$elm_mdl$Material_Options$css = F2(
	function (key, value) {
		return _debois$elm_mdl$Material_Options_Internal$CSS(
			{ctor: '_Tuple2', _0: key, _1: value});
	});
var _debois$elm_mdl$Material_Options$center = _debois$elm_mdl$Material_Options$many(
	{
		ctor: '::',
		_0: A2(_debois$elm_mdl$Material_Options$css, 'display', 'flex'),
		_1: {
			ctor: '::',
			_0: A2(_debois$elm_mdl$Material_Options$css, 'align-items', 'center'),
			_1: {
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Options$css, 'justify-content', 'center'),
				_1: {ctor: '[]'}
			}
		}
	});
var _debois$elm_mdl$Material_Options$scrim = function (opacity) {
	return A2(
		_debois$elm_mdl$Material_Options$css,
		'background',
		A2(
			_elm_lang$core$Basics_ops['++'],
			'linear-gradient(rgba(0, 0, 0, 0), rgba(0, 0, 0, ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(opacity),
				'))')));
};
var _debois$elm_mdl$Material_Options$cs = function (c) {
	return _debois$elm_mdl$Material_Options_Internal$Class(c);
};
var _debois$elm_mdl$Material_Options$disabled = function (v) {
	return _debois$elm_mdl$Material_Options_Internal$Attribute(
		_elm_lang$html$Html_Attributes$disabled(v));
};
var _debois$elm_mdl$Material_Options$styled_ = F3(
	function (ctor, props, attrs) {
		return ctor(
			A2(
				_debois$elm_mdl$Material_Options_Internal$addAttributes,
				_debois$elm_mdl$Material_Options_Internal$collect_(props),
				attrs));
	});
var _debois$elm_mdl$Material_Options$img = F2(
	function (options, attrs) {
		return A4(
			_debois$elm_mdl$Material_Options$styled_,
			_elm_lang$html$Html$img,
			options,
			attrs,
			{ctor: '[]'});
	});
var _debois$elm_mdl$Material_Options$styled = F2(
	function (ctor, props) {
		return ctor(
			A2(
				_debois$elm_mdl$Material_Options_Internal$addAttributes,
				_debois$elm_mdl$Material_Options_Internal$collect_(props),
				{ctor: '[]'}));
	});
var _debois$elm_mdl$Material_Options$div = _debois$elm_mdl$Material_Options$styled(_elm_lang$html$Html$div);
var _debois$elm_mdl$Material_Options$span = _debois$elm_mdl$Material_Options$styled(_elm_lang$html$Html$span);

var _debois$elm_mdl$Material_Ripple$styles = F2(
	function (m, frame) {
		var r = m.rect;
		var toPx = function (k) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(
					_elm_lang$core$Basics$round(k)),
				'px');
		};
		var offset = A2(
			_elm_lang$core$Basics_ops['++'],
			'translate(',
			A2(
				_elm_lang$core$Basics_ops['++'],
				toPx(m.x),
				A2(
					_elm_lang$core$Basics_ops['++'],
					', ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						toPx(m.y),
						')'))));
		var rippleSize = toPx(
			(_elm_lang$core$Basics$sqrt((r.width * r.width) + (r.height * r.height)) * 2.0) + 2.0);
		var scale = _elm_lang$core$Native_Utils.eq(frame, 0) ? 'scale(0.0001, 0.0001)' : '';
		var transformString = A2(
			_elm_lang$core$Basics_ops['++'],
			'translate(-50%, -50%) ',
			A2(_elm_lang$core$Basics_ops['++'], offset, scale));
		return {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'width', _1: rippleSize},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'height', _1: rippleSize},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: '-webkit-transform', _1: transformString},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: '-ms-transform', _1: transformString},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'transform', _1: transformString},
							_1: {ctor: '[]'}
						}
					}
				}
			}
		};
	});
var _debois$elm_mdl$Material_Ripple$Metrics = F3(
	function (a, b, c) {
		return {rect: a, x: b, y: c};
	});
var _debois$elm_mdl$Material_Ripple$computeMetrics = function (g) {
	var rect = g.rect;
	var set = F2(
		function (x, y) {
			return _elm_lang$core$Maybe$Just(
				{ctor: '_Tuple2', _0: x - rect.left, _1: y - rect.top});
		});
	return A2(
		_elm_lang$core$Maybe$map,
		function (_p0) {
			var _p1 = _p0;
			return A3(_debois$elm_mdl$Material_Ripple$Metrics, rect, _p1._0, _p1._1);
		},
		function () {
			var _p2 = {ctor: '_Tuple4', _0: g.clientX, _1: g.clientY, _2: g.touchX, _3: g.touchY};
			_v1_3:
			do {
				if (_p2.ctor === '_Tuple4') {
					if ((_p2._0.ctor === 'Just') && (_p2._1.ctor === 'Just')) {
						if ((_p2._0._0 === 0.0) && (_p2._1._0 === 0.0)) {
							return _elm_lang$core$Maybe$Just(
								{ctor: '_Tuple2', _0: rect.width / 2.0, _1: rect.height / 2.0});
						} else {
							return A2(set, _p2._0._0, _p2._1._0);
						}
					} else {
						if ((_p2._2.ctor === 'Just') && (_p2._3.ctor === 'Just')) {
							return A2(set, _p2._2._0, _p2._3._0);
						} else {
							break _v1_3;
						}
					}
				} else {
					break _v1_3;
				}
			} while(false);
			return _elm_lang$core$Maybe$Nothing;
		}());
};
var _debois$elm_mdl$Material_Ripple$Model = F3(
	function (a, b, c) {
		return {animation: a, metrics: b, ignoringMouseDown: c};
	});
var _debois$elm_mdl$Material_Ripple$DOMState = F6(
	function (a, b, c, d, e, f) {
		return {rect: a, clientX: b, clientY: c, touchX: d, touchY: e, type_: f};
	});
var _debois$elm_mdl$Material_Ripple$geometryDecoder = A7(
	_elm_lang$core$Json_Decode$map6,
	_debois$elm_mdl$Material_Ripple$DOMState,
	A2(_elm_lang$core$Json_Decode$field, 'currentTarget', _debois$elm_dom$DOM$boundingClientRect),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode$field, 'clientX', _elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode$field, 'clientY', _elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(
			_elm_lang$core$Json_Decode$at,
			{
				ctor: '::',
				_0: 'touches',
				_1: {
					ctor: '::',
					_0: '0',
					_1: {
						ctor: '::',
						_0: 'clientX',
						_1: {ctor: '[]'}
					}
				}
			},
			_elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(
			_elm_lang$core$Json_Decode$at,
			{
				ctor: '::',
				_0: 'touches',
				_1: {
					ctor: '::',
					_0: '0',
					_1: {
						ctor: '::',
						_0: 'clientY',
						_1: {ctor: '[]'}
					}
				}
			},
			_elm_lang$core$Json_Decode$float)),
	A2(_elm_lang$core$Json_Decode$field, 'type', _elm_lang$core$Json_Decode$string));
var _debois$elm_mdl$Material_Ripple$Inert = {ctor: 'Inert'};
var _debois$elm_mdl$Material_Ripple$model = {animation: _debois$elm_mdl$Material_Ripple$Inert, metrics: _elm_lang$core$Maybe$Nothing, ignoringMouseDown: false};
var _debois$elm_mdl$Material_Ripple$Frame = function (a) {
	return {ctor: 'Frame', _0: a};
};
var _debois$elm_mdl$Material_Ripple$view_ = F2(
	function (attrs, model) {
		var styling = function () {
			var _p3 = {ctor: '_Tuple2', _0: model.metrics, _1: model.animation};
			if ((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) {
				if (_p3._1.ctor === 'Frame') {
					return A2(_debois$elm_mdl$Material_Ripple$styles, _p3._0._0, _p3._1._0);
				} else {
					return A2(_debois$elm_mdl$Material_Ripple$styles, _p3._0._0, 1);
				}
			} else {
				return {ctor: '[]'};
			}
		}();
		return A2(
			_elm_lang$html$Html$span,
			attrs,
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$span,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$classList(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'mdl-ripple', _1: true},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'is-animating',
										_1: !_elm_lang$core$Native_Utils.eq(
											model.animation,
											_debois$elm_mdl$Material_Ripple$Frame(0))
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: 'is-visible',
											_1: !_elm_lang$core$Native_Utils.eq(model.animation, _debois$elm_mdl$Material_Ripple$Inert)
										},
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(styling),
							_1: {ctor: '[]'}
						}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			});
	});
var _debois$elm_mdl$Material_Ripple$Tick = {ctor: 'Tick'};
var _debois$elm_mdl$Material_Ripple$update = F2(
	function (action, model) {
		var _p4 = action;
		switch (_p4.ctor) {
			case 'Down':
				var _p5 = _p4._0;
				return (_elm_lang$core$Native_Utils.eq(_p5.type_, 'mousedown') && model.ignoringMouseDown) ? _debois$elm_mdl$Material_Helpers$pure(
					_elm_lang$core$Native_Utils.update(
						model,
						{ignoringMouseDown: false})) : A2(
					_debois$elm_mdl$Material_Helpers$effect,
					_debois$elm_mdl$Material_Helpers$cssTransitionStep(_debois$elm_mdl$Material_Ripple$Tick),
					_elm_lang$core$Native_Utils.update(
						model,
						{
							animation: _debois$elm_mdl$Material_Ripple$Frame(0),
							metrics: _debois$elm_mdl$Material_Ripple$computeMetrics(_p5),
							ignoringMouseDown: _elm_lang$core$Native_Utils.eq(_p5.type_, 'touchstart') ? true : model.ignoringMouseDown
						}));
			case 'Up':
				return _debois$elm_mdl$Material_Helpers$pure(
					_elm_lang$core$Native_Utils.update(
						model,
						{animation: _debois$elm_mdl$Material_Ripple$Inert}));
			default:
				return _elm_lang$core$Native_Utils.eq(
					model.animation,
					_debois$elm_mdl$Material_Ripple$Frame(0)) ? _debois$elm_mdl$Material_Helpers$pure(
					_elm_lang$core$Native_Utils.update(
						model,
						{
							animation: _debois$elm_mdl$Material_Ripple$Frame(1)
						})) : _debois$elm_mdl$Material_Helpers$pure(model);
		}
	});
var _debois$elm_mdl$Material_Ripple$Up = {ctor: 'Up'};
var _debois$elm_mdl$Material_Ripple$up = F2(
	function (f, name) {
		return A2(
			_debois$elm_mdl$Material_Options$on,
			name,
			_elm_lang$core$Json_Decode$succeed(
				f(_debois$elm_mdl$Material_Ripple$Up)));
	});
var _debois$elm_mdl$Material_Ripple$upOn_ = F2(
	function (f, name) {
		return A2(
			_elm_lang$html$Html_Events$on,
			name,
			_elm_lang$core$Json_Decode$succeed(
				f(_debois$elm_mdl$Material_Ripple$Up)));
	});
var _debois$elm_mdl$Material_Ripple$upOn = _debois$elm_mdl$Material_Ripple$upOn_(_elm_lang$core$Basics$identity);
var _debois$elm_mdl$Material_Ripple$Down = function (a) {
	return {ctor: 'Down', _0: a};
};
var _debois$elm_mdl$Material_Ripple$downOn_ = F2(
	function (f, name) {
		return A2(
			_elm_lang$html$Html_Events$on,
			name,
			A2(
				_elm_lang$core$Json_Decode$map,
				function (_p6) {
					return f(
						_debois$elm_mdl$Material_Ripple$Down(_p6));
				},
				_debois$elm_mdl$Material_Ripple$geometryDecoder));
	});
var _debois$elm_mdl$Material_Ripple$downOn = _debois$elm_mdl$Material_Ripple$downOn_(_elm_lang$core$Basics$identity);
var _debois$elm_mdl$Material_Ripple$view = function (_p7) {
	return _debois$elm_mdl$Material_Ripple$view_(
		A3(
			_elm_lang$core$Basics$flip,
			_elm_lang$core$List$append,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Ripple$upOn('mouseup'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Ripple$upOn('mouseleave'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Ripple$upOn('touchend'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Ripple$upOn('blur'),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Ripple$downOn('mousedown'),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Ripple$downOn('touchstart'),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			},
			_p7));
};
var _debois$elm_mdl$Material_Ripple$down = F2(
	function (f, name) {
		return A2(
			_debois$elm_mdl$Material_Options$on,
			name,
			A2(
				_elm_lang$core$Json_Decode$map,
				function (_p8) {
					return f(
						_debois$elm_mdl$Material_Ripple$Down(_p8));
				},
				_debois$elm_mdl$Material_Ripple$geometryDecoder));
	});

var _debois$elm_mdl$Material_Button$_p0 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.button;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{button: x});
		}),
	_debois$elm_mdl$Material_Ripple$model);
var _debois$elm_mdl$Material_Button$get = _debois$elm_mdl$Material_Button$_p0._0;
var _debois$elm_mdl$Material_Button$set = _debois$elm_mdl$Material_Button$_p0._1;
var _debois$elm_mdl$Material_Button$icon = _debois$elm_mdl$Material_Options$cs('mdl-button--icon');
var _debois$elm_mdl$Material_Button$minifab = _debois$elm_mdl$Material_Options$cs('mdl-button--mini-fab');
var _debois$elm_mdl$Material_Button$fab = _debois$elm_mdl$Material_Options$cs('mdl-button--fab');
var _debois$elm_mdl$Material_Button$raised = _debois$elm_mdl$Material_Options$cs('mdl-button--raised');
var _debois$elm_mdl$Material_Button$flat = _debois$elm_mdl$Material_Options$nop;
var _debois$elm_mdl$Material_Button$blurAndForward = function (event) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		A2(_elm_lang$core$Basics_ops['++'], 'on', event),
		'this.blur(); (function(self) { var e = document.createEvent(\'Event\'); e.initEvent(\'touchcancel\', true, true); self.lastChild.dispatchEvent(e); }(this));');
};
var _debois$elm_mdl$Material_Button$type_ = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$attribute(
		_elm_lang$html$Html_Attributes$type_(_p1));
};
var _debois$elm_mdl$Material_Button$accent = _debois$elm_mdl$Material_Options$cs('mdl-button--accent');
var _debois$elm_mdl$Material_Button$primary = _debois$elm_mdl$Material_Options$cs('mdl-button--primary');
var _debois$elm_mdl$Material_Button$colored = _debois$elm_mdl$Material_Options$cs('mdl-button--colored');
var _debois$elm_mdl$Material_Button$plain = _debois$elm_mdl$Material_Options$nop;
var _debois$elm_mdl$Material_Button$disabled = _debois$elm_mdl$Material_Options_Internal$attribute(
	_elm_lang$html$Html_Attributes$disabled(true));
var _debois$elm_mdl$Material_Button$ripple = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Button$link = function (href) {
	return _debois$elm_mdl$Material_Options$many(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options_Internal$option(
				function (options) {
					return _elm_lang$core$Native_Utils.update(
						options,
						{link: true});
				}),
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options_Internal$attribute(
					_elm_lang$html$Html_Attributes$href(href)),
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_mdl$Material_Button$defaultConfig = {ripple: false, link: false};
var _debois$elm_mdl$Material_Button$view = F4(
	function (lift, model, config, html) {
		var listeners = _debois$elm_mdl$Material_Options$many(
			{
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Ripple$down, lift, 'mousedown'),
				_1: {
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Ripple$down, lift, 'touchstart'),
					_1: {
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Ripple$up, lift, 'touchcancel'),
						_1: {
							ctor: '::',
							_0: A2(_debois$elm_mdl$Material_Ripple$up, lift, 'mouseup'),
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Ripple$up, lift, 'blur'),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Ripple$up, lift, 'mouseleave'),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			});
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Button$defaultConfig, config);
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			summary.config.link ? _elm_lang$html$Html$a : _elm_lang$html$Html$button,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-button'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-js-button'),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$when,
							summary.config.ripple,
							_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect')),
						_1: {
							ctor: '::',
							_0: listeners,
							_1: {ctor: '[]'}
						}
					}
				}
			},
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Helpers$blurOn('mouseup'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Helpers$blurOn('mouseleave'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Helpers$blurOn('touchend'),
						_1: {ctor: '[]'}
					}
				}
			},
			summary.config.ripple ? _elm_lang$core$List$concat(
				{
					ctor: '::',
					_0: html,
					_1: {
						ctor: '::',
						_0: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$map,
								lift,
								A2(
									_debois$elm_mdl$Material_Ripple$view_,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('mdl-button__ripple-container'),
										_1: {ctor: '[]'}
									},
									model)),
							_1: {ctor: '[]'}
						},
						_1: {ctor: '[]'}
					}
				}) : html);
	});
var _debois$elm_mdl$Material_Button$render = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Button$get, _debois$elm_mdl$Material_Button$view, _debois$elm_mdl$Material_Component$ButtonMsg);
var _debois$elm_mdl$Material_Button$update = function (action) {
	return _debois$elm_mdl$Material_Ripple$update(action);
};
var _debois$elm_mdl$Material_Button$react = A4(
	_debois$elm_mdl$Material_Component$react,
	_debois$elm_mdl$Material_Button$get,
	_debois$elm_mdl$Material_Button$set,
	_debois$elm_mdl$Material_Component$ButtonMsg,
	_debois$elm_mdl$Material_Component$generalise(_debois$elm_mdl$Material_Button$update));
var _debois$elm_mdl$Material_Button$defaultModel = _debois$elm_mdl$Material_Ripple$model;
var _debois$elm_mdl$Material_Button$Config = F2(
	function (a, b) {
		return {ripple: a, link: b};
	});

var _debois$elm_mdl$Material_Icon$view = F2(
	function (name, options) {
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$i,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('material-icons'),
				_1: options
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(name),
				_1: {ctor: '[]'}
			});
	});
var _debois$elm_mdl$Material_Icon$i = function (name) {
	return A2(
		_debois$elm_mdl$Material_Icon$view,
		name,
		{ctor: '[]'});
};
var _debois$elm_mdl$Material_Icon$size48 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '48px');
var _debois$elm_mdl$Material_Icon$size36 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '36px');
var _debois$elm_mdl$Material_Icon$size24 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '24px');
var _debois$elm_mdl$Material_Icon$size18 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '18px');
var _debois$elm_mdl$Material_Icon$defaultConfig = {};
var _debois$elm_mdl$Material_Icon$Config = {};

var _debois$elm_mdl$Material_Textfield$update = F3(
	function (_p0, action, model) {
		return A3(
			_elm_lang$core$Basics$flip,
			F2(
				function (x, y) {
					return A2(_elm_lang$core$Platform_Cmd_ops['!'], x, y);
				}),
			{ctor: '[]'},
			function () {
				var _p1 = action;
				switch (_p1.ctor) {
					case 'Input':
						var dirty = !_elm_lang$core$Native_Utils.eq(_p1._0, '');
						return _elm_lang$core$Native_Utils.eq(dirty, model.isDirty) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
							_elm_lang$core$Native_Utils.update(
								model,
								{isDirty: dirty}));
					case 'Blur':
						return _elm_lang$core$Maybe$Just(
							_elm_lang$core$Native_Utils.update(
								model,
								{isFocused: false}));
					default:
						return _elm_lang$core$Maybe$Just(
							_elm_lang$core$Native_Utils.update(
								model,
								{isFocused: true}));
				}
			}());
	});
var _debois$elm_mdl$Material_Textfield$defaultModel = {isFocused: false, isDirty: false};
var _debois$elm_mdl$Material_Textfield$_p2 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.textfield;
	},
	F2(
		function (x, c) {
			return _elm_lang$core$Native_Utils.update(
				c,
				{textfield: x});
		}),
	_debois$elm_mdl$Material_Textfield$defaultModel);
var _debois$elm_mdl$Material_Textfield$get = _debois$elm_mdl$Material_Textfield$_p2._0;
var _debois$elm_mdl$Material_Textfield$set = _debois$elm_mdl$Material_Textfield$_p2._1;
var _debois$elm_mdl$Material_Textfield$react = A4(_debois$elm_mdl$Material_Component$react, _debois$elm_mdl$Material_Textfield$get, _debois$elm_mdl$Material_Textfield$set, _debois$elm_mdl$Material_Component$TextfieldMsg, _debois$elm_mdl$Material_Textfield$update);
var _debois$elm_mdl$Material_Textfield$cols = function (k) {
	return _debois$elm_mdl$Material_Options_Internal$input(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$attribute(
				_elm_lang$html$Html_Attributes$cols(k)),
			_1: {ctor: '[]'}
		});
};
var _debois$elm_mdl$Material_Textfield$rows = function (k) {
	return _debois$elm_mdl$Material_Options_Internal$input(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$attribute(
				_elm_lang$html$Html_Attributes$rows(k)),
			_1: {ctor: '[]'}
		});
};
var _debois$elm_mdl$Material_Textfield$input = _debois$elm_mdl$Material_Options$input;
var _debois$elm_mdl$Material_Textfield$disabled = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{disabled: true});
	});
var _debois$elm_mdl$Material_Textfield$maxlength = function (k) {
	return _debois$elm_mdl$Material_Options$attribute(
		_elm_lang$html$Html_Attributes$maxlength(k));
};
var _debois$elm_mdl$Material_Textfield$autofocus = _debois$elm_mdl$Material_Options$attribute(
	_elm_lang$html$Html_Attributes$autofocus(true));
var _debois$elm_mdl$Material_Textfield$value = function (_p3) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (str, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						value: _elm_lang$core$Maybe$Just(str)
					});
			})(_p3));
};
var _debois$elm_mdl$Material_Textfield$error = function (_p4) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (str, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						error: _elm_lang$core$Maybe$Just(str)
					});
			})(_p4));
};
var _debois$elm_mdl$Material_Textfield$expandableIcon = function (id) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{expandableIcon: id});
		});
};
var _debois$elm_mdl$Material_Textfield$expandable = function (id) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					expandable: _elm_lang$core$Maybe$Just(id)
				});
		});
};
var _debois$elm_mdl$Material_Textfield$floatingLabel = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{labelFloat: true});
	});
var _debois$elm_mdl$Material_Textfield$label = function (_p5) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (str, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						labelText: _elm_lang$core$Maybe$Just(str)
					});
			})(_p5));
};
var _debois$elm_mdl$Material_Textfield$Config = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {labelText: a, labelFloat: b, error: c, value: d, disabled: e, kind: f, expandable: g, expandableIcon: h, input: i, container: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _debois$elm_mdl$Material_Textfield$Model = F2(
	function (a, b) {
		return {isFocused: a, isDirty: b};
	});
var _debois$elm_mdl$Material_Textfield$Email = {ctor: 'Email'};
var _debois$elm_mdl$Material_Textfield$email = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Email});
	});
var _debois$elm_mdl$Material_Textfield$Password = {ctor: 'Password'};
var _debois$elm_mdl$Material_Textfield$password = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Password});
	});
var _debois$elm_mdl$Material_Textfield$Textarea = {ctor: 'Textarea'};
var _debois$elm_mdl$Material_Textfield$textarea = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Textarea});
	});
var _debois$elm_mdl$Material_Textfield$Text = {ctor: 'Text'};
var _debois$elm_mdl$Material_Textfield$defaultConfig = {
	labelText: _elm_lang$core$Maybe$Nothing,
	labelFloat: false,
	error: _elm_lang$core$Maybe$Nothing,
	value: _elm_lang$core$Maybe$Nothing,
	disabled: false,
	kind: _debois$elm_mdl$Material_Textfield$Text,
	expandable: _elm_lang$core$Maybe$Nothing,
	expandableIcon: 'search',
	input: {ctor: '[]'},
	container: {ctor: '[]'}
};
var _debois$elm_mdl$Material_Textfield$text_ = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Text});
	});
var _debois$elm_mdl$Material_Textfield$Input = function (a) {
	return {ctor: 'Input', _0: a};
};
var _debois$elm_mdl$Material_Textfield$Focus = {ctor: 'Focus'};
var _debois$elm_mdl$Material_Textfield$Blur = {ctor: 'Blur'};
var _debois$elm_mdl$Material_Textfield$view = F4(
	function (lift, model, options, _p6) {
		var _p7 = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Textfield$defaultConfig, options);
		var summary = _p7;
		var config = _p7.config;
		var labelFor = function () {
			var _p8 = config.expandable;
			if (_p8.ctor === 'Nothing') {
				return {ctor: '[]'};
			} else {
				return {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$for(_p8._0),
					_1: {ctor: '[]'}
				};
			}
		}();
		var expandableId = function () {
			var _p9 = config.expandable;
			if (_p9.ctor === 'Nothing') {
				return _debois$elm_mdl$Material_Options$nop;
			} else {
				return _debois$elm_mdl$Material_Options_Internal$attribute(
					_elm_lang$html$Html_Attributes$id(_p9._0));
			}
		}();
		var expHolder = function () {
			var _p10 = config.expandable;
			if (_p10.ctor === 'Nothing') {
				return _elm_lang$core$Basics$identity;
			} else {
				return function (x) {
					return {
						ctor: '::',
						_0: A4(
							_debois$elm_mdl$Material_Options$styled_,
							_elm_lang$html$Html$label,
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$cs('mdl-button'),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$cs('mdl-js-button'),
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Options$cs('mdl-button--icon'),
										_1: {ctor: '[]'}
									}
								}
							},
							labelFor,
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Icon$i(config.expandableIcon),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A3(
								_debois$elm_mdl$Material_Options$styled,
								_elm_lang$html$Html$div,
								{
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$cs('mdl-textfield__expandable-holder'),
									_1: {ctor: '[]'}
								},
								x),
							_1: {ctor: '[]'}
						}
					};
				};
			}
		}();
		return A4(
			_debois$elm_mdl$Material_Options_Internal$applyContainer,
			summary,
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-textfield'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-js-textfield'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('is-upgraded'),
						_1: {
							ctor: '::',
							_0: A3(_debois$elm_mdl$Material_Options_Internal$on1, 'focus', lift, _debois$elm_mdl$Material_Textfield$Focus),
							_1: {
								ctor: '::',
								_0: A3(_debois$elm_mdl$Material_Options_Internal$on1, 'blur', lift, _debois$elm_mdl$Material_Textfield$Blur),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$when,
										config.labelFloat,
										_debois$elm_mdl$Material_Options$cs('mdl-textfield--floating-label')),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Options$when,
											!_elm_lang$core$Native_Utils.eq(config.error, _elm_lang$core$Maybe$Nothing),
											_debois$elm_mdl$Material_Options$cs('is-invalid')),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Options$when,
												function () {
													var _p11 = config.value;
													if (_p11.ctor === 'Just') {
														if (_p11._0 === '') {
															return false;
														} else {
															return true;
														}
													} else {
														return model.isDirty;
													}
												}(),
												_debois$elm_mdl$Material_Options$cs('is-dirty')),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Options$when,
													model.isFocused && (!config.disabled),
													_debois$elm_mdl$Material_Options$cs('is-focused')),
												_1: {
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_Options$when,
														config.disabled,
														_debois$elm_mdl$Material_Options$cs('is-disabled')),
													_1: {
														ctor: '::',
														_0: A2(
															_debois$elm_mdl$Material_Options$when,
															!_elm_lang$core$Native_Utils.eq(config.expandable, _elm_lang$core$Maybe$Nothing),
															_debois$elm_mdl$Material_Options$cs('mdl-textfield--expandable')),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			},
			expHolder(
				{
					ctor: '::',
					_0: A4(
						_debois$elm_mdl$Material_Options_Internal$applyInput,
						summary,
						_elm_lang$core$Native_Utils.eq(config.kind, _debois$elm_mdl$Material_Textfield$Textarea) ? _elm_lang$html$Html$textarea : _elm_lang$html$Html$input,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('mdl-textfield__input'),
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Options$css, 'outline', 'none'),
								_1: {
									ctor: '::',
									_0: A3(_debois$elm_mdl$Material_Options_Internal$on1, 'focus', lift, _debois$elm_mdl$Material_Textfield$Focus),
									_1: {
										ctor: '::',
										_0: A3(_debois$elm_mdl$Material_Options_Internal$on1, 'blur', lift, _debois$elm_mdl$Material_Textfield$Blur),
										_1: {
											ctor: '::',
											_0: function () {
												var _p12 = config.kind;
												switch (_p12.ctor) {
													case 'Text':
														return _debois$elm_mdl$Material_Options_Internal$attribute(
															_elm_lang$html$Html_Attributes$type_('text'));
													case 'Password':
														return _debois$elm_mdl$Material_Options_Internal$attribute(
															_elm_lang$html$Html_Attributes$type_('password'));
													case 'Email':
														return _debois$elm_mdl$Material_Options_Internal$attribute(
															_elm_lang$html$Html_Attributes$type_('email'));
													default:
														return _debois$elm_mdl$Material_Options$nop;
												}
											}(),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Options$when,
													config.disabled,
													_debois$elm_mdl$Material_Options_Internal$attribute(
														_elm_lang$html$Html_Attributes$disabled(true))),
												_1: {
													ctor: '::',
													_0: expandableId,
													_1: {
														ctor: '::',
														_0: function () {
															var _p13 = config.value;
															if (_p13.ctor === 'Nothing') {
																return A2(
																	_debois$elm_mdl$Material_Options$on,
																	'input',
																	A2(
																		_elm_lang$core$Json_Decode$map,
																		function (_p14) {
																			return lift(
																				_debois$elm_mdl$Material_Textfield$Input(_p14));
																		},
																		_elm_lang$html$Html_Events$targetValue));
															} else {
																return _debois$elm_mdl$Material_Options_Internal$attribute(
																	_elm_lang$html$Html_Attributes$value(_p13._0));
															}
														}(),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$label,
							A2(
								_elm_lang$core$Basics_ops['++'],
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('mdl-textfield__label'),
									_1: {ctor: '[]'}
								},
								labelFor),
							function () {
								var _p15 = config.labelText;
								if (_p15.ctor === 'Just') {
									return {
										ctor: '::',
										_0: _elm_lang$html$Html$text(_p15._0),
										_1: {ctor: '[]'}
									};
								} else {
									return {ctor: '[]'};
								}
							}()),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$core$Maybe$withDefault,
								A2(
									_elm_lang$html$Html$div,
									{ctor: '[]'},
									{ctor: '[]'}),
								A2(
									_elm_lang$core$Maybe$map,
									function (e) {
										return A2(
											_elm_lang$html$Html$span,
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$class('mdl-textfield__error'),
												_1: {ctor: '[]'}
											},
											{
												ctor: '::',
												_0: _elm_lang$html$Html$text(e),
												_1: {ctor: '[]'}
											});
									},
									config.error)),
							_1: {ctor: '[]'}
						}
					}
				}));
	});
var _debois$elm_mdl$Material_Textfield$render = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Textfield$get, _debois$elm_mdl$Material_Textfield$view, _debois$elm_mdl$Material_Component$TextfieldMsg);

var _elm_lang$dom$Native_Dom = function() {

var fakeNode = {
	addEventListener: function() {},
	removeEventListener: function() {}
};

var onDocument = on(typeof document !== 'undefined' ? document : fakeNode);
var onWindow = on(typeof window !== 'undefined' ? window : fakeNode);

function on(node)
{
	return function(eventName, decoder, toTask)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

			function performTask(event)
			{
				var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
				if (result.ctor === 'Ok')
				{
					_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
				}
			}

			node.addEventListener(eventName, performTask);

			return function()
			{
				node.removeEventListener(eventName, performTask);
			};
		});
	};
}

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		rAF(function()
		{
			var node = document.getElementById(id);
			if (node === null)
			{
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


// FOCUS

function focus(id)
{
	return withNode(id, function(node) {
		node.focus();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function blur(id)
{
	return withNode(id, function(node) {
		node.blur();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SCROLLING

function getScrollTop(id)
{
	return withNode(id, function(node) {
		return node.scrollTop;
	});
}

function setScrollTop(id, desiredScrollTop)
{
	return withNode(id, function(node) {
		node.scrollTop = desiredScrollTop;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toBottom(id)
{
	return withNode(id, function(node) {
		node.scrollTop = node.scrollHeight;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function getScrollLeft(id)
{
	return withNode(id, function(node) {
		return node.scrollLeft;
	});
}

function setScrollLeft(id, desiredScrollLeft)
{
	return withNode(id, function(node) {
		node.scrollLeft = desiredScrollLeft;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toRight(id)
{
	return withNode(id, function(node) {
		node.scrollLeft = node.scrollWidth;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SIZE

function width(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollWidth;
			case 'VisibleContent':
				return node.clientWidth;
			case 'VisibleContentWithBorders':
				return node.offsetWidth;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.right - rect.left;
		}
	});
}

function height(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollHeight;
			case 'VisibleContent':
				return node.clientHeight;
			case 'VisibleContentWithBorders':
				return node.offsetHeight;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.bottom - rect.top;
		}
	});
}

return {
	onDocument: F3(onDocument),
	onWindow: F3(onWindow),

	focus: focus,
	blur: blur,

	getScrollTop: getScrollTop,
	setScrollTop: F2(setScrollTop),
	getScrollLeft: getScrollLeft,
	setScrollLeft: F2(setScrollLeft),
	toBottom: toBottom,
	toRight: toRight,

	height: F2(height),
	width: F2(width)
};

}();

var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;

var _elm_lang$mouse$Mouse_ops = _elm_lang$mouse$Mouse_ops || {};
_elm_lang$mouse$Mouse_ops['&>'] = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return t2;
			},
			t1);
	});
var _elm_lang$mouse$Mouse$onSelfMsg = F3(
	function (router, _p1, state) {
		var _p2 = _p1;
		var _p3 = A2(_elm_lang$core$Dict$get, _p2.category, state);
		if (_p3.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p2.position));
			};
			return A2(
				_elm_lang$mouse$Mouse_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p3._0.taggers)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$mouse$Mouse$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$mouse$Mouse$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p4 = maybeValues;
		if (_p4.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p4._0});
		}
	});
var _elm_lang$mouse$Mouse$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p5 = subs;
			if (_p5.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p5._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p5._0._0,
					_elm_lang$mouse$Mouse$categorizeHelpHelp(_p5._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$mouse$Mouse$categorize = function (subs) {
	return A2(_elm_lang$mouse$Mouse$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$mouse$Mouse$subscription = _elm_lang$core$Native_Platform.leaf('Mouse');
var _elm_lang$mouse$Mouse$Position = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _elm_lang$mouse$Mouse$position = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$mouse$Mouse$Position,
	A2(_elm_lang$core$Json_Decode$field, 'pageX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'pageY', _elm_lang$core$Json_Decode$int));
var _elm_lang$mouse$Mouse$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$mouse$Mouse$Msg = F2(
	function (a, b) {
		return {category: a, position: b};
	});
var _elm_lang$mouse$Mouse$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				var tracker = A3(
					_elm_lang$dom$Dom_LowLevel$onDocument,
					category,
					_elm_lang$mouse$Mouse$position,
					function (_p6) {
						return A2(
							_elm_lang$core$Platform$sendToSelf,
							router,
							A2(_elm_lang$mouse$Mouse$Msg, category, _p6));
					});
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$mouse$Mouse$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(tracker));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p7, taggers, task) {
				var _p8 = _p7;
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$core$Dict$insert,
								category,
								A2(_elm_lang$mouse$Mouse$Watcher, taggers, _p8.pid),
								state));
					},
					task);
			});
		var leftStep = F3(
			function (category, _p9, task) {
				var _p10 = _p9;
				return A2(
					_elm_lang$mouse$Mouse_ops['&>'],
					_elm_lang$core$Process$kill(_p10.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$mouse$Mouse$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$mouse$Mouse$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$mouse$Mouse$clicks = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'click', tagger));
};
var _elm_lang$mouse$Mouse$moves = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousemove', tagger));
};
var _elm_lang$mouse$Mouse$downs = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousedown', tagger));
};
var _elm_lang$mouse$Mouse$ups = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mouseup', tagger));
};
var _elm_lang$mouse$Mouse$subMap = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return A2(
			_elm_lang$mouse$Mouse$MySub,
			_p12._0,
			function (_p13) {
				return func(
					_p12._1(_p13));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Mouse'] = {pkg: 'elm-lang/mouse', init: _elm_lang$mouse$Mouse$init, onEffects: _elm_lang$mouse$Mouse$onEffects, onSelfMsg: _elm_lang$mouse$Mouse$onSelfMsg, tag: 'sub', subMap: _elm_lang$mouse$Mouse$subMap};

var _debois$elm_mdl$Material_Menu_Geometry$Geometry = F5(
	function (a, b, c, d, e) {
		return {button: a, menu: b, container: c, offsetTops: d, offsetHeights: e};
	});
var _debois$elm_mdl$Material_Menu_Geometry$Element = F4(
	function (a, b, c, d) {
		return {offsetTop: a, offsetLeft: b, offsetHeight: c, bounds: d};
	});
var _debois$elm_mdl$Material_Menu_Geometry$element = A5(_elm_lang$core$Json_Decode$map4, _debois$elm_mdl$Material_Menu_Geometry$Element, _debois$elm_dom$DOM$offsetTop, _debois$elm_dom$DOM$offsetLeft, _debois$elm_dom$DOM$offsetHeight, _debois$elm_dom$DOM$boundingClientRect);
var _debois$elm_mdl$Material_Menu_Geometry$decode = A6(
	_elm_lang$core$Json_Decode$map5,
	_debois$elm_mdl$Material_Menu_Geometry$Geometry,
	_debois$elm_dom$DOM$target(_debois$elm_mdl$Material_Menu_Geometry$element),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(
			A2(_debois$elm_dom$DOM$childNode, 1, _debois$elm_mdl$Material_Menu_Geometry$element))),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(_debois$elm_mdl$Material_Menu_Geometry$element)),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(
			A2(
				_debois$elm_dom$DOM$childNode,
				1,
				_debois$elm_dom$DOM$childNodes(_debois$elm_dom$DOM$offsetTop)))),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(
			A2(
				_debois$elm_dom$DOM$childNode,
				1,
				_debois$elm_dom$DOM$childNodes(_debois$elm_dom$DOM$offsetHeight)))));

var _debois$elm_mdl$Material_Menu$toPx = function (_p0) {
	return A3(
		_elm_lang$core$Basics$flip,
		F2(
			function (x, y) {
				return A2(_elm_lang$core$Basics_ops['++'], x, y);
			}),
		'px',
		_elm_lang$core$Basics$toString(_p0));
};
var _debois$elm_mdl$Material_Menu$rect = F4(
	function (x, y, w, h) {
		return function (coords) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'rect(',
				A2(_elm_lang$core$Basics_ops['++'], coords, ')'));
		}(
			A2(
				_elm_lang$core$String$join,
				' ',
				A2(
					_elm_lang$core$List$map,
					_debois$elm_mdl$Material_Menu$toPx,
					{
						ctor: '::',
						_0: x,
						_1: {
							ctor: '::',
							_0: y,
							_1: {
								ctor: '::',
								_0: w,
								_1: {
									ctor: '::',
									_0: h,
									_1: {ctor: '[]'}
								}
							}
						}
					})));
	});
var _debois$elm_mdl$Material_Menu$onKeyDown = function (action) {
	return A3(
		_debois$elm_mdl$Material_Options$onWithOptions,
		'keydown',
		{preventDefault: true, stopPropagation: false},
		A2(_elm_lang$core$Json_Decode$map, action, _elm_lang$html$Html_Events$keyCode));
};
var _debois$elm_mdl$Material_Menu$onClick = F2(
	function (decoder, action) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'click',
			A2(_elm_lang$core$Json_Decode$map, action, decoder));
	});
var _debois$elm_mdl$Material_Menu$withGeometry = F2(
	function (model, f) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			_debois$elm_mdl$Material_Options$nop,
			A2(_elm_lang$core$Maybe$map, f, model.geometry));
	});
var _debois$elm_mdl$Material_Menu$icon = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (name, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{icon: name});
			})(_p1));
};
var _debois$elm_mdl$Material_Menu$ripple = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Menu$onSelect = function (_p2) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (msg, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						onSelect: _elm_lang$core$Maybe$Just(msg)
					});
			})(_p2));
};
var _debois$elm_mdl$Material_Menu$disabled = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{enabled: false});
	});
var _debois$elm_mdl$Material_Menu$divider = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{divider: true});
	});
var _debois$elm_mdl$Material_Menu$defaultItemConfig = {enabled: true, divider: false, onSelect: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Menu$constant = {transitionDurationSeconds: 0.3, transitionDurationFraction: 0.8, closeTimeout: 150};
var _debois$elm_mdl$Material_Menu$transitionDuration = _debois$elm_mdl$Material_Menu$constant.transitionDurationSeconds * _debois$elm_mdl$Material_Menu$constant.transitionDurationFraction;
var _debois$elm_mdl$Material_Menu$Model = F4(
	function (a, b, c, d) {
		return {ripples: a, animationState: b, geometry: c, index: d};
	});
var _debois$elm_mdl$Material_Menu$Item = F2(
	function (a, b) {
		return {options: a, html: b};
	});
var _debois$elm_mdl$Material_Menu$item = _debois$elm_mdl$Material_Menu$Item;
var _debois$elm_mdl$Material_Menu$ItemConfig = F3(
	function (a, b, c) {
		return {enabled: a, divider: b, onSelect: c};
	});
var _debois$elm_mdl$Material_Menu$Config = F3(
	function (a, b, c) {
		return {alignment: a, ripple: b, icon: c};
	});
var _debois$elm_mdl$Material_Menu$Closing = {ctor: 'Closing'};
var _debois$elm_mdl$Material_Menu$Opened = {ctor: 'Opened'};
var _debois$elm_mdl$Material_Menu$clip = F3(
	function (model, config, geometry) {
		var height = geometry.menu.bounds.height;
		var width = geometry.menu.bounds.width;
		return A2(
			_debois$elm_mdl$Material_Options$css,
			'clip',
			function () {
				if (_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Closing)) {
					return A4(_debois$elm_mdl$Material_Menu$rect, 0, width, height, 0);
				} else {
					var _p3 = config.alignment;
					switch (_p3.ctor) {
						case 'BottomRight':
							return A4(_debois$elm_mdl$Material_Menu$rect, 0, width, 0, width);
						case 'TopLeft':
							return A4(_debois$elm_mdl$Material_Menu$rect, height, 0, height, 0);
						case 'TopRight':
							return A4(_debois$elm_mdl$Material_Menu$rect, height, width, height, width);
						default:
							return '';
					}
				}
			}());
	});
var _debois$elm_mdl$Material_Menu$Opening = {ctor: 'Opening'};
var _debois$elm_mdl$Material_Menu$isActive = function (model) {
	return _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opening);
};
var _debois$elm_mdl$Material_Menu$Idle = {ctor: 'Idle'};
var _debois$elm_mdl$Material_Menu$defaultModel = {ripples: _elm_lang$core$Dict$empty, animationState: _debois$elm_mdl$Material_Menu$Idle, geometry: _elm_lang$core$Maybe$Nothing, index: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Menu$_p4 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.menu;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{menu: x});
		}),
	_debois$elm_mdl$Material_Menu$defaultModel);
var _debois$elm_mdl$Material_Menu$get = _debois$elm_mdl$Material_Menu$_p4._0;
var _debois$elm_mdl$Material_Menu$set = _debois$elm_mdl$Material_Menu$_p4._1;
var _debois$elm_mdl$Material_Menu$Key = F2(
	function (a, b) {
		return {ctor: 'Key', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Menu$Click = function (a) {
	return {ctor: 'Click', _0: a};
};
var _debois$elm_mdl$Material_Menu$subscriptions = function (model) {
	return _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) ? _elm_lang$mouse$Mouse$clicks(_debois$elm_mdl$Material_Menu$Click) : _elm_lang$core$Platform_Sub$none;
};
var _debois$elm_mdl$Material_Menu$subs = A3(
	_debois$elm_mdl$Material_Component$subs,
	_debois$elm_mdl$Material_Component$MenuMsg,
	function (_) {
		return _.menu;
	},
	_debois$elm_mdl$Material_Menu$subscriptions);
var _debois$elm_mdl$Material_Menu$Ripple = F2(
	function (a, b) {
		return {ctor: 'Ripple', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Menu$Tick = {ctor: 'Tick'};
var _debois$elm_mdl$Material_Menu$Close = {ctor: 'Close'};
var _debois$elm_mdl$Material_Menu$Select = F2(
	function (a, b) {
		return {ctor: 'Select', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Menu$update = F3(
	function (fwd, msg, model) {
		update:
		while (true) {
			var _p5 = msg;
			switch (_p5.ctor) {
				case 'Open':
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								animationState: function () {
									var _p6 = model.animationState;
									if (_p6.ctor === 'Opened') {
										return _debois$elm_mdl$Material_Menu$Opened;
									} else {
										return _debois$elm_mdl$Material_Menu$Opening;
									}
								}(),
								geometry: _elm_lang$core$Maybe$Just(_p5._0)
							}),
						_1: _debois$elm_mdl$Material_Helpers$cmd(
							fwd(_debois$elm_mdl$Material_Menu$Tick))
					};
				case 'Tick':
					return _debois$elm_mdl$Material_Helpers$pure(
						_elm_lang$core$Native_Utils.update(
							model,
							{animationState: _debois$elm_mdl$Material_Menu$Opened}));
				case 'Close':
					return _debois$elm_mdl$Material_Helpers$pure(
						_elm_lang$core$Native_Utils.update(
							model,
							{animationState: _debois$elm_mdl$Material_Menu$Idle, geometry: _elm_lang$core$Maybe$Nothing, index: _elm_lang$core$Maybe$Nothing}));
				case 'Select':
					var cmds = A2(
						_elm_lang$core$List$filterMap,
						_elm_lang$core$Basics$identity,
						{
							ctor: '::',
							_0: _elm_lang$core$Maybe$Just(
								A2(
									_debois$elm_mdl$Material_Helpers$delay,
									_debois$elm_mdl$Material_Menu$constant.closeTimeout,
									fwd(_debois$elm_mdl$Material_Menu$Close))),
							_1: {
								ctor: '::',
								_0: A2(_elm_lang$core$Maybe$map, _debois$elm_mdl$Material_Helpers$cmd, _p5._1),
								_1: {ctor: '[]'}
							}
						});
					var model_ = _elm_lang$core$Native_Utils.update(
						model,
						{animationState: _debois$elm_mdl$Material_Menu$Closing});
					return {
						ctor: '_Tuple2',
						_0: model_,
						_1: _elm_lang$core$Platform_Cmd$batch(cmds)
					};
				case 'Ripple':
					var _p9 = _p5._0;
					var _p7 = A2(
						_debois$elm_mdl$Material_Ripple$update,
						_p5._1,
						A2(
							_elm_lang$core$Maybe$withDefault,
							_debois$elm_mdl$Material_Ripple$model,
							A2(_elm_lang$core$Dict$get, _p9, model.ripples)));
					var model_ = _p7._0;
					var effects = _p7._1;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								ripples: A3(_elm_lang$core$Dict$insert, _p9, model_, model.ripples)
							}),
						_1: A2(
							_elm_lang$core$Platform_Cmd$map,
							function (_p8) {
								return fwd(
									A2(_debois$elm_mdl$Material_Menu$Ripple, _p9, _p8));
							},
							effects)
					};
				case 'Click':
					if (_debois$elm_mdl$Material_Menu$isActive(model)) {
						var _p10 = model.geometry;
						if (_p10.ctor === 'Just') {
							var inside = F2(
								function (_p12, _p11) {
									var _p13 = _p12;
									var _p18 = _p13.y;
									var _p17 = _p13.x;
									var _p14 = _p11;
									var _p16 = _p14.top;
									var _p15 = _p14.left;
									return (_elm_lang$core$Native_Utils.cmp(
										_p15,
										_elm_lang$core$Basics$toFloat(_p17)) < 1) && ((_elm_lang$core$Native_Utils.cmp(
										_elm_lang$core$Basics$toFloat(_p17),
										_p15 + _p14.width) < 1) && ((_elm_lang$core$Native_Utils.cmp(
										_p16,
										_elm_lang$core$Basics$toFloat(_p18)) < 1) && (_elm_lang$core$Native_Utils.cmp(
										_elm_lang$core$Basics$toFloat(_p18),
										_p16 + _p14.height) < 1)));
								});
							if (A2(inside, _p5._0, _p10._0.menu.bounds)) {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									{ctor: '[]'});
							} else {
								var _v6 = fwd,
									_v7 = _debois$elm_mdl$Material_Menu$Close,
									_v8 = model;
								fwd = _v6;
								msg = _v7;
								model = _v8;
								continue update;
							}
						} else {
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								{ctor: '[]'});
						}
					} else {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					}
				default:
					var _p27 = _p5._0;
					var _p19 = _p5._1;
					switch (_p19) {
						case 13:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var _p20 = model.index;
								if (_p20.ctor === 'Just') {
									var _p22 = _p20._0;
									var cmd = A2(
										_elm_lang$core$Maybe$andThen,
										function (_p21) {
											return function (_) {
												return _.onSelect;
											}(
												function (_) {
													return _.config;
												}(_p21));
										},
										_elm_lang$core$List$head(
											A2(_elm_lang$core$List$drop, _p22, _p27)));
									var _v11 = fwd,
										_v12 = A2(_debois$elm_mdl$Material_Menu$Select, _p22 + 1, cmd),
										_v13 = model;
									fwd = _v11;
									msg = _v12;
									model = _v13;
									continue update;
								} else {
									var _v14 = fwd,
										_v15 = _debois$elm_mdl$Material_Menu$Close,
										_v16 = model;
									fwd = _v14;
									msg = _v15;
									model = _v16;
									continue update;
								}
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									{ctor: '[]'});
							}
						case 27:
							var _v17 = fwd,
								_v18 = _debois$elm_mdl$Material_Menu$Close,
								_v19 = model;
							fwd = _v17;
							msg = _v18;
							model = _v19;
							continue update;
						case 32:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var _v20 = fwd,
									_v21 = A2(_debois$elm_mdl$Material_Menu$Key, _p27, 13),
									_v22 = model;
								fwd = _v20;
								msg = _v21;
								model = _v22;
								continue update;
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									{ctor: '[]'});
							}
						case 40:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var items = A2(
									_elm_lang$core$List$indexedMap,
									F2(
										function (v0, v1) {
											return {ctor: '_Tuple2', _0: v0, _1: v1};
										}),
									_p27);
								return A3(
									_elm_lang$core$Basics$flip,
									F2(
										function (x, y) {
											return A2(_elm_lang$core$Platform_Cmd_ops['!'], x, y);
										}),
									{ctor: '[]'},
									A2(
										_elm_lang$core$Maybe$withDefault,
										model,
										A2(
											_elm_lang$core$Maybe$map,
											function (_p23) {
												return function (index_) {
													return _elm_lang$core$Native_Utils.update(
														model,
														{
															index: _elm_lang$core$Maybe$Just(index_)
														});
												}(
													_elm_lang$core$Tuple$first(_p23));
											},
											_elm_lang$core$List$head(
												A2(
													_elm_lang$core$List$filter,
													function (_p24) {
														return function (_) {
															return _.enabled;
														}(
															function (_) {
																return _.config;
															}(
																_elm_lang$core$Tuple$second(_p24)));
													},
													A2(
														_elm_lang$core$List$drop,
														1 + A2(_elm_lang$core$Maybe$withDefault, -1, model.index),
														A2(_elm_lang$core$Basics_ops['++'], items, items)))))));
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									{ctor: '[]'});
							}
						case 38:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var items = A2(
									_elm_lang$core$List$indexedMap,
									F2(
										function (v0, v1) {
											return {ctor: '_Tuple2', _0: v0, _1: v1};
										}),
									_p27);
								return _debois$elm_mdl$Material_Helpers$pure(
									A2(
										_elm_lang$core$Maybe$withDefault,
										model,
										A2(
											_elm_lang$core$Maybe$map,
											function (_p25) {
												return function (index_) {
													return _elm_lang$core$Native_Utils.update(
														model,
														{
															index: _elm_lang$core$Maybe$Just(index_)
														});
												}(
													_elm_lang$core$Tuple$first(_p25));
											},
											_elm_lang$core$List$head(
												A2(
													_elm_lang$core$List$filter,
													function (_p26) {
														return function (_) {
															return _.enabled;
														}(
															function (_) {
																return _.config;
															}(
																_elm_lang$core$Tuple$second(_p26)));
													},
													A2(
														_elm_lang$core$List$drop,
														_elm_lang$core$List$length(_p27) - A2(_elm_lang$core$Maybe$withDefault, 0, model.index),
														_elm_lang$core$List$reverse(
															A2(_elm_lang$core$Basics_ops['++'], items, items))))))));
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									{ctor: '[]'});
							}
						default:
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								{ctor: '[]'});
					}
			}
		}
	});
var _debois$elm_mdl$Material_Menu$react = F4(
	function (lift, msg, idx, store) {
		return A2(
			_debois$elm_mdl$Material_Helpers$map1st,
			function (_p28) {
				return _elm_lang$core$Maybe$Just(
					A3(_debois$elm_mdl$Material_Menu$set, idx, store, _p28));
			},
			A3(
				_debois$elm_mdl$Material_Menu$update,
				lift,
				msg,
				A2(_debois$elm_mdl$Material_Menu$get, idx, store)));
	});
var _debois$elm_mdl$Material_Menu$Open = function (a) {
	return {ctor: 'Open', _0: a};
};
var _debois$elm_mdl$Material_Menu$TopRight = {ctor: 'TopRight'};
var _debois$elm_mdl$Material_Menu$topRight = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$TopRight});
	});
var _debois$elm_mdl$Material_Menu$TopLeft = {ctor: 'TopLeft'};
var _debois$elm_mdl$Material_Menu$topLeft = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$TopLeft});
	});
var _debois$elm_mdl$Material_Menu$delay = F4(
	function (alignment, height, offsetTop, offsetHeight) {
		var t = (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopLeft) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopRight)) ? ((((height - offsetTop) - offsetHeight) / height) * _debois$elm_mdl$Material_Menu$transitionDuration) : ((offsetTop / height) * _debois$elm_mdl$Material_Menu$transitionDuration);
		return A2(
			_debois$elm_mdl$Material_Options$css,
			'transition-delay',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(t),
				's'));
	});
var _debois$elm_mdl$Material_Menu$view1 = F8(
	function (lift, config, model, offsetTop, offsetHeight, index, summary, item) {
		var canSelect = summary.config.enabled && (!_elm_lang$core$Native_Utils.eq(summary.config.onSelect, _elm_lang$core$Maybe$Nothing));
		var hasRipple = config.ripple && canSelect;
		var ripple = function (_p29) {
			return lift(
				A2(_debois$elm_mdl$Material_Menu$Ripple, index, _p29));
		};
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$li,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-menu__item'),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$when,
						config.ripple,
						_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect')),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$when,
							summary.config.divider,
							_debois$elm_mdl$Material_Options$cs('mdl-menu__item--full-bleed-divider')),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								_elm_lang$core$Native_Utils.eq(
									model.index,
									_elm_lang$core$Maybe$Just(index)),
								A2(_debois$elm_mdl$Material_Options$css, 'background-color', 'rgb(238,238,238)')),
							_1: {
								ctor: '::',
								_0: function () {
									var _p30 = {
										ctor: '_Tuple2',
										_0: model.geometry,
										_1: _debois$elm_mdl$Material_Menu$isActive(model)
									};
									if (((_p30.ctor === '_Tuple2') && (_p30._0.ctor === 'Just')) && (_p30._1 === true)) {
										return A4(_debois$elm_mdl$Material_Menu$delay, config.alignment, _p30._0._0.menu.bounds.height, offsetTop, offsetHeight);
									} else {
										return _debois$elm_mdl$Material_Options$nop;
									}
								}(),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'display', 'flex'),
									_1: {
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Options$css, 'align-items', 'center'),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Options$when,
												canSelect,
												_debois$elm_mdl$Material_Options$onClick(
													lift(
														A2(_debois$elm_mdl$Material_Menu$Select, index, summary.config.onSelect)))),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Options$when,
													!summary.config.enabled,
													_debois$elm_mdl$Material_Options_Internal$attribute(
														A2(_elm_lang$html$Html_Attributes$attribute, 'disabled', 'disabled'))),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options_Internal$attribute(
														A2(
															_elm_lang$html$Html_Attributes$property,
															'tabindex',
															_elm_lang$core$Json_Encode$string('-1'))),
													_1: {
														ctor: '::',
														_0: hasRipple ? _debois$elm_mdl$Material_Options$many(
															{
																ctor: '::',
																_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																	A2(_debois$elm_mdl$Material_Ripple$downOn_, ripple, 'mousedown')),
																_1: {
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																		A2(_debois$elm_mdl$Material_Ripple$downOn_, ripple, 'touchstart')),
																	_1: {
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																			A2(_debois$elm_mdl$Material_Ripple$upOn_, ripple, 'mouseup')),
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																				A2(_debois$elm_mdl$Material_Ripple$upOn_, ripple, 'mouseleave')),
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																					A2(_debois$elm_mdl$Material_Ripple$upOn_, ripple, 'touchend')),
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																						A2(_debois$elm_mdl$Material_Ripple$upOn_, ripple, 'blur')),
																					_1: {ctor: '[]'}
																				}
																			}
																		}
																	}
																}
															}) : _debois$elm_mdl$Material_Options$nop,
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			},
			{ctor: '[]'},
			hasRipple ? A2(
				F2(
					function (x, y) {
						return A2(_elm_lang$core$Basics_ops['++'], x, y);
					}),
				item.html,
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$map,
						ripple,
						A2(
							_debois$elm_mdl$Material_Ripple$view_,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('mdl-menu__item-ripple-container'),
								_1: {ctor: '[]'}
							},
							A2(
								_elm_lang$core$Maybe$withDefault,
								_debois$elm_mdl$Material_Ripple$model,
								A2(_elm_lang$core$Dict$get, index, model.ripples)))),
					_1: {ctor: '[]'}
				}) : item.html);
	});
var _debois$elm_mdl$Material_Menu$BottomRight = {ctor: 'BottomRight'};
var _debois$elm_mdl$Material_Menu$bottomRight = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$BottomRight});
	});
var _debois$elm_mdl$Material_Menu$BottomLeft = {ctor: 'BottomLeft'};
var _debois$elm_mdl$Material_Menu$defaultConfig = {alignment: _debois$elm_mdl$Material_Menu$BottomLeft, ripple: false, icon: 'more_vert'};
var _debois$elm_mdl$Material_Menu$bottomLeft = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$BottomLeft});
	});
var _debois$elm_mdl$Material_Menu$containerGeometry = F2(
	function (alignment, geometry) {
		return _debois$elm_mdl$Material_Options$many(
			{
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Options$css,
					'width',
					_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.width)),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$css,
						'height',
						_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.height)),
					_1: {
						ctor: '::',
						_0: (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomRight) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomLeft)) ? A2(
							_debois$elm_mdl$Material_Options$css,
							'top',
							_debois$elm_mdl$Material_Menu$toPx(geometry.button.offsetTop + geometry.button.offsetHeight)) : _debois$elm_mdl$Material_Options$nop,
						_1: {
							ctor: '::',
							_0: function () {
								if (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomRight) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopRight)) {
									var right = function (e) {
										return e.bounds.left + e.bounds.width;
									};
									return A2(
										_debois$elm_mdl$Material_Options$css,
										'right',
										_debois$elm_mdl$Material_Menu$toPx(
											right(geometry.container) - right(geometry.menu)));
								} else {
									return _debois$elm_mdl$Material_Options$nop;
								}
							}(),
							_1: {
								ctor: '::',
								_0: function () {
									if (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopLeft) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopRight)) {
										var bottom = geometry.container.bounds.top + geometry.container.bounds.height;
										return A2(
											_debois$elm_mdl$Material_Options$css,
											'bottom',
											_debois$elm_mdl$Material_Menu$toPx(bottom - geometry.button.bounds.top));
									} else {
										return _debois$elm_mdl$Material_Options$nop;
									}
								}(),
								_1: {
									ctor: '::',
									_0: (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopLeft) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomLeft)) ? A2(
										_debois$elm_mdl$Material_Options$css,
										'left',
										_debois$elm_mdl$Material_Menu$toPx(geometry.menu.offsetLeft)) : _debois$elm_mdl$Material_Options$nop,
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			});
	});
var _debois$elm_mdl$Material_Menu$view = F4(
	function (lift, model, properties, items) {
		var itemSummaries = A2(
			_elm_lang$core$List$map,
			function (_p31) {
				return A2(
					_debois$elm_mdl$Material_Options_Internal$collect,
					_debois$elm_mdl$Material_Menu$defaultItemConfig,
					function (_) {
						return _.options;
					}(_p31));
			},
			items);
		var numItems = _elm_lang$core$List$length(items);
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Menu$defaultConfig, properties);
		var config = summary.config;
		var alignment = function () {
			var _p32 = config.alignment;
			switch (_p32.ctor) {
				case 'BottomLeft':
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--bottom-left');
				case 'BottomRight':
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--bottom-right');
				case 'TopLeft':
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--top-left');
				default:
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--top-right');
			}
		}();
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Options$css, 'position', 'relative'),
				_1: properties
			},
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$map,
					lift,
					A3(
						_debois$elm_mdl$Material_Options$styled,
						_elm_lang$html$Html$button,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('mdl-button'),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$cs('mdl-js-button'),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$cs('mdl-button--icon'),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Options$when,
											_debois$elm_mdl$Material_Menu$isActive(model),
											_debois$elm_mdl$Material_Menu$onKeyDown(
												_debois$elm_mdl$Material_Menu$Key(itemSummaries))),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Options$when,
												!_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened),
												A2(
													_debois$elm_mdl$Material_Options$on,
													'click',
													A2(_elm_lang$core$Json_Decode$map, _debois$elm_mdl$Material_Menu$Open, _debois$elm_mdl$Material_Menu_Geometry$decode))),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Options$when,
													_debois$elm_mdl$Material_Menu$isActive(model),
													_debois$elm_mdl$Material_Options$onClick(_debois$elm_mdl$Material_Menu$Close)),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						},
						{
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Icon$view,
								config.icon,
								{
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$cs('material-icons'),
									_1: {
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Options$css, 'pointer-events', 'none'),
										_1: {ctor: '[]'}
									}
								}),
							_1: {ctor: '[]'}
						})),
				_1: {
					ctor: '::',
					_0: A3(
						_debois$elm_mdl$Material_Options$styled,
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('mdl-menu__container'),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$cs('is-upgraded'),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$when,
										_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Closing),
										_debois$elm_mdl$Material_Options$cs('is-visible')),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Menu$withGeometry,
											model,
											_debois$elm_mdl$Material_Menu$containerGeometry(config.alignment)),
										_1: {ctor: '[]'}
									}
								}
							}
						},
						{
							ctor: '::',
							_0: A3(
								_debois$elm_mdl$Material_Options$styled,
								_elm_lang$html$Html$div,
								{
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$cs('mdl-menu__outline'),
									_1: {
										ctor: '::',
										_0: alignment,
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Menu$withGeometry,
												model,
												function (geometry) {
													return _debois$elm_mdl$Material_Options$many(
														{
															ctor: '::',
															_0: A2(
																_debois$elm_mdl$Material_Options$css,
																'width',
																_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.width)),
															_1: {
																ctor: '::',
																_0: A2(
																	_debois$elm_mdl$Material_Options$css,
																	'height',
																	_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.height)),
																_1: {ctor: '[]'}
															}
														});
												}),
											_1: {ctor: '[]'}
										}
									}
								},
								{ctor: '[]'}),
							_1: {
								ctor: '::',
								_0: A3(
									_debois$elm_mdl$Material_Options$styled,
									_elm_lang$html$Html$ul,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Options$cs('mdl-menu'),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Options$cs('mdl-js-menu'),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Options$when,
													_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opening) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Closing),
													_debois$elm_mdl$Material_Options$cs('is-animating')),
												_1: {
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_Menu$withGeometry,
														model,
														A2(_debois$elm_mdl$Material_Menu$clip, model, config)),
													_1: {
														ctor: '::',
														_0: alignment,
														_1: {ctor: '[]'}
													}
												}
											}
										}
									},
									function () {
										var _p33 = model.geometry;
										if (_p33.ctor === 'Just') {
											var _p34 = _p33._0;
											return A6(
												_elm_lang$core$List$map5,
												A3(_debois$elm_mdl$Material_Menu$view1, lift, config, model),
												_p34.offsetTops,
												_p34.offsetHeights,
												A2(_elm_lang$core$List$range, 0, numItems - 1),
												itemSummaries,
												items);
										} else {
											return A4(
												_elm_lang$core$List$map3,
												A5(_debois$elm_mdl$Material_Menu$view1, lift, config, model, 0, 0),
												A2(_elm_lang$core$List$range, 0, numItems - 1),
												itemSummaries,
												items);
										}
									}()),
								_1: {ctor: '[]'}
							}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_mdl$Material_Menu$render = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Menu$get, _debois$elm_mdl$Material_Menu$view, _debois$elm_mdl$Material_Component$MenuMsg);

var _debois$elm_mdl$Material_Snackbar$enqueue = F2(
	function (contents, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				queue: A2(
					_elm_lang$core$List$append,
					model.queue,
					{
						ctor: '::',
						_0: contents,
						_1: {ctor: '[]'}
					})
			});
	});
var _debois$elm_mdl$Material_Snackbar$snackbar = F3(
	function (payload, message, label) {
		return {
			message: message,
			action: _elm_lang$core$Maybe$Just(label),
			payload: payload,
			timeout: 2750,
			fade: 250
		};
	});
var _debois$elm_mdl$Material_Snackbar$toast = F2(
	function (payload, message) {
		return {message: message, action: _elm_lang$core$Maybe$Nothing, payload: payload, timeout: 2750, fade: 250};
	});
var _debois$elm_mdl$Material_Snackbar$Contents = F5(
	function (a, b, c, d, e) {
		return {message: a, action: b, payload: c, timeout: d, fade: e};
	});
var _debois$elm_mdl$Material_Snackbar$Model = F3(
	function (a, b, c) {
		return {queue: a, state: b, seq: c};
	});
var _debois$elm_mdl$Material_Snackbar$Fading = function (a) {
	return {ctor: 'Fading', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$Active = function (a) {
	return {ctor: 'Active', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$Inert = {ctor: 'Inert'};
var _debois$elm_mdl$Material_Snackbar$model = {
	queue: {ctor: '[]'},
	state: _debois$elm_mdl$Material_Snackbar$Inert,
	seq: -1
};
var _debois$elm_mdl$Material_Snackbar$Clicked = {ctor: 'Clicked'};
var _debois$elm_mdl$Material_Snackbar$Timeout = {ctor: 'Timeout'};
var _debois$elm_mdl$Material_Snackbar$Move = F2(
	function (a, b) {
		return {ctor: 'Move', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Snackbar$next = function (model) {
	return _elm_lang$core$Platform_Cmd$map(
		_debois$elm_mdl$Material_Snackbar$Move(model.seq));
};
var _debois$elm_mdl$Material_Snackbar$view = function (model) {
	var isActive = function () {
		var _p0 = model.state;
		switch (_p0.ctor) {
			case 'Inert':
				return false;
			case 'Active':
				return true;
			default:
				return false;
		}
	}();
	var contents = function () {
		var _p1 = model.state;
		switch (_p1.ctor) {
			case 'Inert':
				return _elm_lang$core$Maybe$Nothing;
			case 'Active':
				return _elm_lang$core$Maybe$Just(_p1._0);
			default:
				return _elm_lang$core$Maybe$Just(_p1._0);
		}
	}();
	var action = A2(
		_elm_lang$core$Maybe$andThen,
		function (_) {
			return _.action;
		},
		contents);
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$classList(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'mdl-js-snackbar', _1: true},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'mdl-snackbar', _1: true},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'mdl-snackbar--active', _1: isActive},
							_1: {ctor: '[]'}
						}
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Helpers$aria, 'hidden', !isActive),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('mdl-snackbar__text'),
					_1: {ctor: '[]'}
				},
				A2(
					_elm_lang$core$Maybe$withDefault,
					{ctor: '[]'},
					A2(
						_elm_lang$core$Maybe$map,
						function (c) {
							return {
								ctor: '::',
								_0: _elm_lang$html$Html$text(c.message),
								_1: {ctor: '[]'}
							};
						},
						contents))),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$button,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('mdl-snackbar__action'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$type_('button'),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Helpers$aria,
									'hidden',
									A2(
										_elm_lang$core$Maybe$withDefault,
										true,
										A2(
											_elm_lang$core$Maybe$map,
											_elm_lang$core$Basics$always(!isActive),
											action))),
								_1: A2(
									_elm_lang$core$Maybe$withDefault,
									{ctor: '[]'},
									A2(
										_elm_lang$core$Maybe$map,
										_elm_lang$core$Basics$always(
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Events$onClick(
													A2(_debois$elm_mdl$Material_Snackbar$Move, model.seq, _debois$elm_mdl$Material_Snackbar$Clicked)),
												_1: {ctor: '[]'}
											}),
										action))
							}
						}
					},
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (action) {
								return {
									ctor: '::',
									_0: _elm_lang$html$Html$text(action),
									_1: {ctor: '[]'}
								};
							},
							action))),
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_mdl$Material_Snackbar$Click = function (a) {
	return {ctor: 'Click', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$End = function (a) {
	return {ctor: 'End', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$Begin = function (a) {
	return {ctor: 'Begin', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$tryDequeue = function (model) {
	var _p2 = {ctor: '_Tuple2', _0: model.state, _1: model.queue};
	if (((_p2.ctor === '_Tuple2') && (_p2._0.ctor === 'Inert')) && (_p2._1.ctor === '::')) {
		var _p3 = _p2._1._0;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				model,
				{
					state: _debois$elm_mdl$Material_Snackbar$Active(_p3),
					queue: _p2._1._1,
					seq: model.seq + 1
				}),
			_1: _elm_lang$core$Platform_Cmd$batch(
				{
					ctor: '::',
					_0: A2(
						_elm_lang$core$Platform_Cmd$map,
						_debois$elm_mdl$Material_Snackbar$Move(model.seq + 1),
						A2(_debois$elm_mdl$Material_Helpers$delay, _p3.timeout, _debois$elm_mdl$Material_Snackbar$Timeout)),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Helpers$cmd(
							_debois$elm_mdl$Material_Snackbar$Begin(_p3.payload)),
						_1: {ctor: '[]'}
					}
				})
		};
	} else {
		return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
	}
};
var _debois$elm_mdl$Material_Snackbar$move = F2(
	function (transition, model) {
		var _p4 = {ctor: '_Tuple2', _0: model.state, _1: transition};
		_v3_4:
		do {
			if (_p4.ctor === '_Tuple2') {
				if (_p4._1.ctor === 'Clicked') {
					if (_p4._0.ctor === 'Active') {
						var _p5 = _p4._0._0;
						return {
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								model,
								{
									state: _debois$elm_mdl$Material_Snackbar$Fading(_p5)
								}),
							_1: _elm_lang$core$Platform_Cmd$batch(
								{
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Snackbar$next,
										model,
										A2(_debois$elm_mdl$Material_Helpers$delay, _p5.fade, _debois$elm_mdl$Material_Snackbar$Timeout)),
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Helpers$cmd(
											_debois$elm_mdl$Material_Snackbar$Click(_p5.payload)),
										_1: {ctor: '[]'}
									}
								})
						};
					} else {
						break _v3_4;
					}
				} else {
					switch (_p4._0.ctor) {
						case 'Inert':
							return _debois$elm_mdl$Material_Snackbar$tryDequeue(model);
						case 'Active':
							var _p6 = _p4._0._0;
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Native_Utils.update(
									model,
									{
										state: _debois$elm_mdl$Material_Snackbar$Fading(_p6)
									}),
								_1: _elm_lang$core$Platform_Cmd$batch(
									{
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Snackbar$next,
											model,
											A2(_debois$elm_mdl$Material_Helpers$delay, _p6.fade, _debois$elm_mdl$Material_Snackbar$Timeout)),
										_1: {ctor: '[]'}
									})
							};
						default:
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Native_Utils.update(
									model,
									{state: _debois$elm_mdl$Material_Snackbar$Inert}),
								_1: _elm_lang$core$Platform_Cmd$batch(
									{
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Snackbar$next,
											model,
											_debois$elm_mdl$Material_Helpers$cmd(_debois$elm_mdl$Material_Snackbar$Timeout)),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Helpers$cmd(
												_debois$elm_mdl$Material_Snackbar$End(_p4._0._0.payload)),
											_1: {ctor: '[]'}
										}
									})
							};
					}
				}
			} else {
				break _v3_4;
			}
		} while(false);
		return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
	});
var _debois$elm_mdl$Material_Snackbar$update = F2(
	function (action, model) {
		var _p7 = action;
		if (_p7.ctor === 'Move') {
			return _elm_lang$core$Native_Utils.eq(_p7._0, model.seq) ? A2(_debois$elm_mdl$Material_Snackbar$move, _p7._1, model) : {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
		} else {
			return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
		}
	});
var _debois$elm_mdl$Material_Snackbar$add = F2(
	function (contents, model) {
		return _debois$elm_mdl$Material_Snackbar$tryDequeue(
			A2(_debois$elm_mdl$Material_Snackbar$enqueue, contents, model));
	});

var _elm_lang$html$Html_Keyed$node = _elm_lang$virtual_dom$VirtualDom$keyedNode;
var _elm_lang$html$Html_Keyed$ol = _elm_lang$html$Html_Keyed$node('ol');
var _elm_lang$html$Html_Keyed$ul = _elm_lang$html$Html_Keyed$node('ul');

var _elm_lang$window$Native_Window = function()
{

var size = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)	{
	callback(_elm_lang$core$Native_Scheduler.succeed({
		width: window.innerWidth,
		height: window.innerHeight
	}));
});

return {
	size: size
};

}();
var _elm_lang$window$Window_ops = _elm_lang$window$Window_ops || {};
_elm_lang$window$Window_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return task2;
			},
			task1);
	});
var _elm_lang$window$Window$onSelfMsg = F3(
	function (router, dimensions, state) {
		var _p1 = state;
		if (_p1.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (_p2) {
				var _p3 = _p2;
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					_p3._0(dimensions));
			};
			return A2(
				_elm_lang$window$Window_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p1._0.subs)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$window$Window$init = _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
var _elm_lang$window$Window$size = _elm_lang$window$Native_Window.size;
var _elm_lang$window$Window$width = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.width;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$height = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.height;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$onEffects = F3(
	function (router, newSubs, oldState) {
		var _p4 = {ctor: '_Tuple2', _0: oldState, _1: newSubs};
		if (_p4._0.ctor === 'Nothing') {
			if (_p4._1.ctor === '[]') {
				return _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
			} else {
				return A2(
					_elm_lang$core$Task$andThen,
					function (pid) {
						return _elm_lang$core$Task$succeed(
							_elm_lang$core$Maybe$Just(
								{subs: newSubs, pid: pid}));
					},
					_elm_lang$core$Process$spawn(
						A3(
							_elm_lang$dom$Dom_LowLevel$onWindow,
							'resize',
							_elm_lang$core$Json_Decode$succeed(
								{ctor: '_Tuple0'}),
							function (_p5) {
								return A2(
									_elm_lang$core$Task$andThen,
									_elm_lang$core$Platform$sendToSelf(router),
									_elm_lang$window$Window$size);
							})));
			}
		} else {
			if (_p4._1.ctor === '[]') {
				return A2(
					_elm_lang$window$Window_ops['&>'],
					_elm_lang$core$Process$kill(_p4._0._0.pid),
					_elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing));
			} else {
				return _elm_lang$core$Task$succeed(
					_elm_lang$core$Maybe$Just(
						{subs: newSubs, pid: _p4._0._0.pid}));
			}
		}
	});
var _elm_lang$window$Window$subscription = _elm_lang$core$Native_Platform.leaf('Window');
var _elm_lang$window$Window$Size = F2(
	function (a, b) {
		return {width: a, height: b};
	});
var _elm_lang$window$Window$MySub = function (a) {
	return {ctor: 'MySub', _0: a};
};
var _elm_lang$window$Window$resizes = function (tagger) {
	return _elm_lang$window$Window$subscription(
		_elm_lang$window$Window$MySub(tagger));
};
var _elm_lang$window$Window$subMap = F2(
	function (func, _p6) {
		var _p7 = _p6;
		return _elm_lang$window$Window$MySub(
			function (_p8) {
				return func(
					_p7._0(_p8));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Window'] = {pkg: 'elm-lang/window', init: _elm_lang$window$Window$init, onEffects: _elm_lang$window$Window$onEffects, onSelfMsg: _elm_lang$window$Window$onSelfMsg, tag: 'sub', subMap: _elm_lang$window$Window$subMap};

var _debois$elm_mdl$Material_Layout$_p0 = {
	ctor: '_Tuple2',
	_0: function (_) {
		return _.layout;
	},
	_1: F2(
		function (x, s) {
			return _elm_lang$core$Native_Utils.update(
				s,
				{layout: x});
		})
};
var _debois$elm_mdl$Material_Layout$get = _debois$elm_mdl$Material_Layout$_p0._0;
var _debois$elm_mdl$Material_Layout$set = _debois$elm_mdl$Material_Layout$_p0._1;
var _debois$elm_mdl$Material_Layout$drawerView = F3(
	function (lift, isVisible, elems) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'mdl-layout__drawer', _1: true},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'is-visible', _1: isVisible},
							_1: {ctor: '[]'}
						}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html_Attributes$attribute,
						'aria-hidden',
						isVisible ? 'false' : 'true'),
					_1: {ctor: '[]'}
				}
			},
			elems);
	});
var _debois$elm_mdl$Material_Layout$onKeypressFilterSpaceAndEnter = A2(_elm_lang$html$Html_Attributes$attribute, 'onkeypress', '\n  (function (evt) {\n     if (evt && evt.type === \"keydown\" && (evt.keyCode === 32 || evt.keyCode === 13)) {\n       evt.preventDefault();\n     }\n   })(window.event);\n  ');
var _debois$elm_mdl$Material_Layout$toList = function (x) {
	var _p1 = x;
	if (_p1.ctor === 'Nothing') {
		return {ctor: '[]'};
	} else {
		return {
			ctor: '::',
			_0: _p1._0,
			_1: {ctor: '[]'}
		};
	}
};
var _debois$elm_mdl$Material_Layout$isWaterfall = function (mode) {
	var _p2 = mode;
	if (_p2.ctor === 'Waterfall') {
		return true;
	} else {
		return false;
	}
};
var _debois$elm_mdl$Material_Layout$row = function (styles) {
	return _debois$elm_mdl$Material_Options$div(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__header-row'),
			_1: styles
		});
};
var _debois$elm_mdl$Material_Layout$link = F2(
	function (styles, contents) {
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$a,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-navigation__link'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options_Internal$attribute(
						A2(_elm_lang$html$Html_Attributes$attribute, 'tabindex', '1')),
					_1: styles
				}
			},
			contents);
	});
var _debois$elm_mdl$Material_Layout$href = function (url) {
	return _debois$elm_mdl$Material_Options$attribute(
		_elm_lang$html$Html_Attributes$href(url));
};
var _debois$elm_mdl$Material_Layout$navigation = F2(
	function (styles, contents) {
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$nav,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-navigation'),
				_1: styles
			},
			contents);
	});
var _debois$elm_mdl$Material_Layout$title = function (styles) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__title'),
			_1: styles
		});
};
var _debois$elm_mdl$Material_Layout$spacer = A2(
	_elm_lang$html$Html$div,
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('mdl-layout-spacer'),
		_1: {ctor: '[]'}
	},
	{ctor: '[]'});
var _debois$elm_mdl$Material_Layout$onSelectTab = function (_p3) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (f, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						onSelectTab: _elm_lang$core$Maybe$Just(
							function (_p4) {
								return _elm_lang$html$Html_Events$onClick(
									f(_p4));
							})
					});
			})(_p3));
};
var _debois$elm_mdl$Material_Layout$moreTabs = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{moreTabs: true});
	});
var _debois$elm_mdl$Material_Layout$selectedTab = function (_p5) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (k, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{selectedTab: k});
			})(_p5));
};
var _debois$elm_mdl$Material_Layout$transparentHeader = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{transparentHeader: true});
	});
var _debois$elm_mdl$Material_Layout$rippleTabs = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{rippleTabs: true});
	});
var _debois$elm_mdl$Material_Layout$fixedTabs = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{fixedTabs: true});
	});
var _debois$elm_mdl$Material_Layout$fixedDrawer = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{fixedDrawer: true});
	});
var _debois$elm_mdl$Material_Layout$fixedHeader = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{fixedHeader: true});
	});
var _debois$elm_mdl$Material_Layout$mainId = 'elm-mdl-layout-main';
var _debois$elm_mdl$Material_Layout$setTabsWidth_ = F2(
	function (width, model) {
		var x = model.tabScrollState;
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				tabScrollState: _elm_lang$core$Native_Utils.update(
					x,
					{
						width: _elm_lang$core$Maybe$Just(width)
					})
			});
	});
var _debois$elm_mdl$Material_Layout$setTabsWidth = F2(
	function (w, container) {
		return _elm_lang$core$Native_Utils.update(
			container,
			{
				layout: A2(_debois$elm_mdl$Material_Layout$setTabsWidth_, w, container.layout)
			});
	});
var _debois$elm_mdl$Material_Layout$defaultTabScrollState = {canScrollRight: true, canScrollLeft: false, width: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Layout$defaultModel = {ripples: _elm_lang$core$Dict$empty, isSmallScreen: false, isCompact: false, isAnimating: false, isScrolled: false, isDrawerOpen: false, tabScrollState: _debois$elm_mdl$Material_Layout$defaultTabScrollState};
var _debois$elm_mdl$Material_Layout$TabScrollState = F3(
	function (a, b, c) {
		return {canScrollLeft: a, canScrollRight: b, width: c};
	});
var _debois$elm_mdl$Material_Layout$Model = F7(
	function (a, b, c, d, e, f, g) {
		return {ripples: a, isSmallScreen: b, isCompact: c, isAnimating: d, isScrolled: e, isDrawerOpen: f, tabScrollState: g};
	});
var _debois$elm_mdl$Material_Layout$Config = F9(
	function (a, b, c, d, e, f, g, h, i) {
		return {fixedHeader: a, fixedDrawer: b, fixedTabs: c, rippleTabs: d, mode: e, selectedTab: f, onSelectTab: g, transparentHeader: h, moreTabs: i};
	});
var _debois$elm_mdl$Material_Layout$Contents = F4(
	function (a, b, c, d) {
		return {header: a, drawer: b, tabs: c, main: d};
	});
var _debois$elm_mdl$Material_Layout$Ripple = F2(
	function (a, b) {
		return {ctor: 'Ripple', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Layout$NOP = {ctor: 'NOP'};
var _debois$elm_mdl$Material_Layout$TransitionEnd = {ctor: 'TransitionEnd'};
var _debois$elm_mdl$Material_Layout$TransitionHeader = function (a) {
	return {ctor: 'TransitionHeader', _0: a};
};
var _debois$elm_mdl$Material_Layout$update_ = F3(
	function (f, action, model) {
		update_:
		while (true) {
			var _p6 = action;
			switch (_p6.ctor) {
				case 'NOP':
					return _elm_lang$core$Maybe$Nothing;
				case 'Resize':
					var _p7 = _p6._0;
					var tabScrollState = A2(
						_elm_lang$core$Maybe$withDefault,
						model.tabScrollState,
						A2(
							_elm_lang$core$Maybe$map,
							function (tabsWidth) {
								var tabScrollState = model.tabScrollState;
								return _elm_lang$core$Native_Utils.update(
									tabScrollState,
									{
										canScrollRight: _elm_lang$core$Native_Utils.cmp(tabsWidth + (2 * 56), _p7) > 0
									});
							},
							model.tabScrollState.width));
					var isSmall = _elm_lang$core$Native_Utils.cmp(1024, _p7) > -1;
					return (_elm_lang$core$Native_Utils.eq(isSmall, model.isSmallScreen) && _elm_lang$core$Native_Utils.eq(tabScrollState.canScrollRight, model.tabScrollState.canScrollRight)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{isSmallScreen: isSmall, isDrawerOpen: (!isSmall) && model.isDrawerOpen, tabScrollState: tabScrollState})));
				case 'ToggleDrawer':
					return _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{isDrawerOpen: !model.isDrawerOpen})));
				case 'Ripple':
					var _p9 = _p6._0;
					return _elm_lang$core$Maybe$Just(
						A2(
							_debois$elm_mdl$Material_Helpers$map2nd,
							_elm_lang$core$Platform_Cmd$map(
								function (_p8) {
									return f(
										A2(_debois$elm_mdl$Material_Layout$Ripple, _p9, _p8));
								}),
							A2(
								_debois$elm_mdl$Material_Helpers$map1st,
								function (ripple_) {
									return _elm_lang$core$Native_Utils.update(
										model,
										{
											ripples: A3(_elm_lang$core$Dict$insert, _p9, ripple_, model.ripples)
										});
								},
								A2(
									_debois$elm_mdl$Material_Ripple$update,
									_p6._1,
									A2(
										_elm_lang$core$Maybe$withDefault,
										_debois$elm_mdl$Material_Ripple$model,
										A2(_elm_lang$core$Dict$get, _p9, model.ripples))))));
				case 'ScrollTab':
					var _p10 = _p6._0;
					return (!_elm_lang$core$Native_Utils.eq(model.tabScrollState, _p10)) ? _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{tabScrollState: _p10}))) : _elm_lang$core$Maybe$Nothing;
				case 'ScrollPane':
					var isScrolled = _elm_lang$core$Native_Utils.cmp(0.0, _p6._1) < 0;
					if (!_elm_lang$core$Native_Utils.eq(isScrolled, model.isScrolled)) {
						var _v3 = f,
							_v4 = _debois$elm_mdl$Material_Layout$TransitionHeader(
							{toCompact: isScrolled, fixedHeader: _p6._0}),
							_v5 = _elm_lang$core$Native_Utils.update(
							model,
							{isScrolled: isScrolled});
						f = _v3;
						action = _v4;
						model = _v5;
						continue update_;
					} else {
						return _elm_lang$core$Maybe$Nothing;
					}
				case 'TransitionHeader':
					return (!model.isAnimating) ? _elm_lang$core$Maybe$Just(
						{
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								model,
								{isCompact: _p6._0.toCompact, isAnimating: (!model.isSmallScreen) || _p6._0.fixedHeader}),
							_1: _elm_lang$core$Platform_Cmd$none
						}) : _elm_lang$core$Maybe$Nothing;
				default:
					return _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{isAnimating: false})));
			}
		}
	});
var _debois$elm_mdl$Material_Layout$update = F2(
	function (msg, model) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none},
			A3(_debois$elm_mdl$Material_Layout$update_, _elm_lang$core$Basics$identity, msg, model));
	});
var _debois$elm_mdl$Material_Layout$react = F3(
	function (lift, msg, store) {
		var _p11 = A3(
			_debois$elm_mdl$Material_Layout$update_,
			lift,
			msg,
			_debois$elm_mdl$Material_Layout$get(store));
		if ((_p11.ctor === 'Just') && (_p11._0.ctor === '_Tuple2')) {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Maybe$Just(
					A2(_debois$elm_mdl$Material_Layout$set, _p11._0._0, store)),
				_1: _p11._0._1
			};
		} else {
			return {ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: _elm_lang$core$Platform_Cmd$none};
		}
	});
var _debois$elm_mdl$Material_Layout$ScrollPane = F2(
	function (a, b) {
		return {ctor: 'ScrollPane', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Layout$ScrollTab = function (a) {
	return {ctor: 'ScrollTab', _0: a};
};
var _debois$elm_mdl$Material_Layout$Resize = function (a) {
	return {ctor: 'Resize', _0: a};
};
var _debois$elm_mdl$Material_Layout$init = function () {
	var measureScreenSize = A2(_elm_lang$core$Task$perform, _debois$elm_mdl$Material_Layout$Resize, _elm_lang$window$Window$width);
	return {ctor: '_Tuple2', _0: _debois$elm_mdl$Material_Layout$defaultModel, _1: measureScreenSize};
}();
var _debois$elm_mdl$Material_Layout$sub0 = function (lift) {
	return A2(
		_elm_lang$core$Platform_Cmd$map,
		function (_p12) {
			return lift(
				_debois$elm_mdl$Material_Component$LayoutMsg(_p12));
		},
		_elm_lang$core$Tuple$second(_debois$elm_mdl$Material_Layout$init));
};
var _debois$elm_mdl$Material_Layout$subscriptions = function (model) {
	return _elm_lang$window$Window$resizes(
		function (_p13) {
			return _debois$elm_mdl$Material_Layout$Resize(
				function (_) {
					return _.width;
				}(_p13));
		});
};
var _debois$elm_mdl$Material_Layout$subs = function (lift) {
	return function (_p14) {
		return A2(
			_elm_lang$core$Platform_Sub$map,
			function (_p15) {
				return lift(
					_debois$elm_mdl$Material_Component$LayoutMsg(_p15));
			},
			_debois$elm_mdl$Material_Layout$subscriptions(
				_debois$elm_mdl$Material_Layout$get(_p14)));
	};
};
var _debois$elm_mdl$Material_Layout$ToggleDrawer = {ctor: 'ToggleDrawer'};
var _debois$elm_mdl$Material_Layout$drawerButton = F2(
	function (lift, isVisible) {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$classList(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'mdl-layout__drawer-button', _1: true},
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html_Attributes$attribute,
								'aria-expanded',
								isVisible ? 'true' : 'false'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$tabindex(1),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Events$onClick(
										lift(_debois$elm_mdl$Material_Layout$ToggleDrawer)),
									_1: {
										ctor: '::',
										_0: A3(
											_elm_lang$html$Html_Events$onWithOptions,
											'keydown',
											{stopPropagation: false, preventDefault: false},
											A2(
												_elm_lang$core$Json_Decode$map,
												function (_p16) {
													return lift(
														function (key) {
															var _p17 = key;
															switch (_p17) {
																case 32:
																	return _debois$elm_mdl$Material_Layout$ToggleDrawer;
																case 13:
																	return _debois$elm_mdl$Material_Layout$ToggleDrawer;
																default:
																	return _debois$elm_mdl$Material_Layout$NOP;
															}
														}(_p16));
												},
												_elm_lang$html$Html_Events$keyCode)),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					},
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Icon$i('menu'),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	});
var _debois$elm_mdl$Material_Layout$obfuscator = F2(
	function (lift, isVisible) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'mdl-layout__obfuscator', _1: true},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'is-visible', _1: isVisible},
							_1: {ctor: '[]'}
						}
					}),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onClick(
						lift(_debois$elm_mdl$Material_Layout$ToggleDrawer)),
					_1: {ctor: '[]'}
				}
			},
			{ctor: '[]'});
	});
var _debois$elm_mdl$Material_Layout$toggleDrawer = function (lift) {
	return function (_p18) {
		return lift(
			_debois$elm_mdl$Material_Component$LayoutMsg(_p18));
	}(_debois$elm_mdl$Material_Layout$ToggleDrawer);
};
var _debois$elm_mdl$Material_Layout$Waterfall = function (a) {
	return {ctor: 'Waterfall', _0: a};
};
var _debois$elm_mdl$Material_Layout$waterfall = function (_p19) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (b, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						mode: _debois$elm_mdl$Material_Layout$Waterfall(b)
					});
			})(_p19));
};
var _debois$elm_mdl$Material_Layout$Scrolling = {ctor: 'Scrolling'};
var _debois$elm_mdl$Material_Layout$scrolling = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{mode: _debois$elm_mdl$Material_Layout$Scrolling});
	});
var _debois$elm_mdl$Material_Layout$Seamed = {ctor: 'Seamed'};
var _debois$elm_mdl$Material_Layout$seamed = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{mode: _debois$elm_mdl$Material_Layout$Seamed});
	});
var _debois$elm_mdl$Material_Layout$Standard = {ctor: 'Standard'};
var _debois$elm_mdl$Material_Layout$defaultConfig = {fixedHeader: false, fixedDrawer: false, fixedTabs: false, rippleTabs: true, mode: _debois$elm_mdl$Material_Layout$Standard, onSelectTab: _elm_lang$core$Maybe$Nothing, selectedTab: -1, moreTabs: false, transparentHeader: false};
var _debois$elm_mdl$Material_Layout$headerView = F4(
	function (lift, config, model, _p20) {
		var _p21 = _p20;
		var mode = function () {
			var _p22 = config.mode;
			switch (_p22.ctor) {
				case 'Standard':
					return _debois$elm_mdl$Material_Options$nop;
				case 'Scrolling':
					return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--scroll');
				case 'Seamed':
					return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--seamed');
				default:
					if (_p22._0 === true) {
						return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--waterfall mdl-layout__header--waterfall-hide-top');
					} else {
						return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--waterfall');
					}
			}
		}();
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$header,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__header'),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$when,
						_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Standard) || (_debois$elm_mdl$Material_Layout$isWaterfall(config.mode) && model.isCompact),
						_debois$elm_mdl$Material_Options$cs('is-casting-shadow')),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$when,
							model.isAnimating,
							_debois$elm_mdl$Material_Options$cs('is-animating')),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								model.isCompact,
								_debois$elm_mdl$Material_Options$cs('is-compact')),
							_1: {
								ctor: '::',
								_0: mode,
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$when,
										config.transparentHeader,
										_debois$elm_mdl$Material_Options$cs('mdl-layout__header--transparent')),
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Options$onClick(
											lift(
												_debois$elm_mdl$Material_Layout$TransitionHeader(
													{toCompact: false, fixedHeader: config.fixedHeader}))),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Options$on,
												'transitionend',
												_elm_lang$core$Json_Decode$succeed(
													lift(_debois$elm_mdl$Material_Layout$TransitionEnd))),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}
			},
			A2(
				_elm_lang$core$List$concatMap,
				function (x) {
					return x;
				},
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Layout$toList(_p21._0),
					_1: {
						ctor: '::',
						_0: _p21._1,
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Layout$toList(_p21._2),
							_1: {ctor: '[]'}
						}
					}
				}));
	});
var _debois$elm_mdl$Material_Layout$Right = {ctor: 'Right'};
var _debois$elm_mdl$Material_Layout$Left = {ctor: 'Left'};
var _debois$elm_mdl$Material_Layout$tabsView = F4(
	function (lift, config, model, _p23) {
		var _p24 = _p23;
		var _p27 = _p24._1;
		var chevron = F2(
			function (direction, offset) {
				var dir = function () {
					var _p25 = direction;
					if (_p25.ctor === 'Left') {
						return 'left';
					} else {
						return 'right';
					}
				}();
				return A3(
					_debois$elm_mdl$Material_Options$styled,
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__tab-bar-button'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs(
								A2(
									_elm_lang$core$Basics_ops['++'],
									'mdl-layout__tab-bar-',
									A2(_elm_lang$core$Basics_ops['++'], dir, '-button'))),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Options$when,
									(_elm_lang$core$Native_Utils.eq(direction, _debois$elm_mdl$Material_Layout$Left) && model.tabScrollState.canScrollLeft) || (_elm_lang$core$Native_Utils.eq(direction, _debois$elm_mdl$Material_Layout$Right) && model.tabScrollState.canScrollRight),
									_debois$elm_mdl$Material_Options$cs('is-active')),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$many(_p27),
									_1: {ctor: '[]'}
								}
							}
						}
					},
					{
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Icon$view,
							A2(_elm_lang$core$Basics_ops['++'], 'chevron_', dir),
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Icon$size24,
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options_Internal$attribute(
										A2(
											_elm_lang$html$Html_Attributes$attribute,
											'onclick',
											A2(
												_elm_lang$core$Basics_ops['++'],
												'document.getElementsByClassName(\'mdl-layout__tab-bar\')[0].scrollLeft += ',
												_elm_lang$core$Basics$toString(offset)))),
									_1: {ctor: '[]'}
								}
							}),
						_1: {ctor: '[]'}
					});
			});
		return A2(
			_debois$elm_mdl$Material_Options$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__tab-bar-container'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(chevron, _debois$elm_mdl$Material_Layout$Left, -100),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$div,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__tab-bar'),
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Options$css, 'position', 'relative'),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'scroll-behavior', 'smooth'),
									_1: {
										ctor: '::',
										_0: config.rippleTabs ? _debois$elm_mdl$Material_Options$many(
											{
												ctor: '::',
												_0: _debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect'),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options$cs('mds-js-ripple-effect--ignore-events'),
													_1: {ctor: '[]'}
												}
											}) : _debois$elm_mdl$Material_Options$nop,
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Standard) ? _debois$elm_mdl$Material_Options$cs('is-casting-shadow') : _debois$elm_mdl$Material_Options$nop,
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Options$many(_p27),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options_Internal$attribute(
														A2(
															_elm_lang$html$Html_Events$on,
															'scroll',
															_debois$elm_dom$DOM$target(
																A4(
																	_elm_lang$core$Json_Decode$map3,
																	F3(
																		function (scrollWidth, clientWidth, scrollLeft) {
																			return lift(
																				_debois$elm_mdl$Material_Layout$ScrollTab(
																					{
																						canScrollLeft: _elm_lang$core$Native_Utils.cmp(scrollLeft, 0) > 0,
																						canScrollRight: _elm_lang$core$Native_Utils.cmp(scrollWidth - clientWidth, scrollLeft + 1) > 0,
																						width: _elm_lang$core$Maybe$Just(scrollWidth)
																					}));
																		}),
																	A2(_elm_lang$core$Json_Decode$field, 'scrollWidth', _elm_lang$core$Json_Decode$float),
																	A2(_elm_lang$core$Json_Decode$field, 'clientWidth', _elm_lang$core$Json_Decode$float),
																	A2(_elm_lang$core$Json_Decode$field, 'scrollLeft', _elm_lang$core$Json_Decode$float))))),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						},
						A2(
							_elm_lang$core$List$indexedMap,
							F2(
								function (tabIndex, tab) {
									return A3(
										_debois$elm_mdl$Material_Helpers$filter,
										_elm_lang$html$Html$a,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$classList(
												{
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'mdl-layout__tab', _1: true},
													_1: {
														ctor: '::',
														_0: {
															ctor: '_Tuple2',
															_0: 'is-active',
															_1: _elm_lang$core$Native_Utils.eq(tabIndex, config.selectedTab)
														},
														_1: {ctor: '[]'}
													}
												}),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$core$Maybe$withDefault,
													_debois$elm_mdl$Material_Helpers$noAttr,
													A2(
														_elm_lang$core$Maybe$map,
														F2(
															function (x, y) {
																return y(x);
															})(tabIndex),
														config.onSelectTab)),
												_1: {ctor: '[]'}
											}
										},
										{
											ctor: '::',
											_0: _elm_lang$core$Maybe$Just(tab),
											_1: {
												ctor: '::',
												_0: config.rippleTabs ? _elm_lang$core$Maybe$Just(
													A2(
														_elm_lang$html$Html$map,
														function (_p26) {
															return lift(
																A2(_debois$elm_mdl$Material_Layout$Ripple, tabIndex, _p26));
														},
														A2(
															_debois$elm_mdl$Material_Ripple$view,
															{
																ctor: '::',
																_0: _elm_lang$html$Html_Attributes$class('mdl-layout__tab-ripple-container'),
																_1: {ctor: '[]'}
															},
															A2(
																_elm_lang$core$Maybe$withDefault,
																_debois$elm_mdl$Material_Ripple$model,
																A2(_elm_lang$core$Dict$get, tabIndex, model.ripples))))) : _elm_lang$core$Maybe$Nothing,
												_1: {ctor: '[]'}
											}
										});
								}),
							_p24._0)),
					_1: {
						ctor: '::',
						_0: A2(chevron, _debois$elm_mdl$Material_Layout$Right, 100),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _debois$elm_mdl$Material_Layout$view = F4(
	function (lift, model, options, _p28) {
		var _p29 = _p28;
		var _p37 = _p29.tabs;
		var _p36 = _p29.header;
		var _p35 = _p29.drawer;
		var hasDrawer = !_elm_lang$core$Native_Utils.eq(
			_p35,
			{ctor: '[]'});
		var hasTabs = !_elm_lang$core$List$isEmpty(
			_elm_lang$core$Tuple$first(_p37));
		var hasHeader = hasTabs || (!_elm_lang$core$List$isEmpty(_p36));
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Layout$defaultConfig, options);
		var config = summary.config;
		var drawerIsFixed = config.fixedDrawer && (!model.isSmallScreen);
		var drawerIsVisible = model.isDrawerOpen && (!drawerIsFixed);
		var _p30 = function () {
			var _p31 = {ctor: '_Tuple3', _0: _p35, _1: _p36, _2: config.fixedHeader};
			if ((_p31.ctor === '_Tuple3') && (_p31._0.ctor === '::')) {
				if ((_p31._1.ctor === '::') && (_p31._2 === true)) {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Nothing,
						_1: _elm_lang$core$Maybe$Just(
							A2(_debois$elm_mdl$Material_Layout$drawerButton, lift, drawerIsVisible))
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(
							A2(_debois$elm_mdl$Material_Layout$drawerButton, lift, drawerIsVisible)),
						_1: _elm_lang$core$Maybe$Nothing
					};
				}
			} else {
				return {ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: _elm_lang$core$Maybe$Nothing};
			}
		}();
		var contentDrawerButton = _p30._0;
		var headerDrawerButton = _p30._1;
		var tabsElems = (!hasTabs) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
			A4(_debois$elm_mdl$Material_Layout$tabsView, lift, config, model, _p37));
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'mdl-layout__container', _1: true},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'has-scrolling-header',
								_1: _elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling)
							},
							_1: {ctor: '[]'}
						}
					}),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A3(
					_debois$elm_mdl$Material_Helpers$filter,
					_elm_lang$html$Html_Keyed$node('div'),
					A2(
						_elm_lang$core$List$filterMap,
						_elm_lang$core$Basics$identity,
						{
							ctor: '::',
							_0: _elm_lang$core$Maybe$Just(
								_elm_lang$html$Html_Attributes$classList(
									{
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'mdl-layout ', _1: true},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'is-upgraded', _1: true},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'is-small-screen', _1: model.isSmallScreen},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'has-drawer', _1: hasDrawer},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'has-tabs', _1: hasTabs},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'mdl-js-layout', _1: true},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'mdl-layout--fixed-drawer', _1: config.fixedDrawer && hasDrawer},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'mdl-layout--fixed-header', _1: config.fixedHeader && hasHeader},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'mdl-layout--fixed-tabs', _1: config.fixedTabs && hasTabs},
																		_1: {ctor: '[]'}
																	}
																}
															}
														}
													}
												}
											}
										}
									})),
							_1: {
								ctor: '::',
								_0: drawerIsVisible ? _elm_lang$core$Maybe$Just(
									A2(
										_elm_lang$html$Html_Events$on,
										'keydown',
										A2(
											_elm_lang$core$Json_Decode$map,
											function (_p32) {
												return lift(
													function (key) {
														return _elm_lang$core$Native_Utils.eq(key, 27) ? _debois$elm_mdl$Material_Layout$ToggleDrawer : _debois$elm_mdl$Material_Layout$NOP;
													}(_p32));
											},
											_elm_lang$html$Html_Events$keyCode))) : _elm_lang$core$Maybe$Nothing,
								_1: {ctor: '[]'}
							}
						}),
					{
						ctor: '::',
						_0: hasHeader ? _elm_lang$core$Maybe$Just(
							A2(
								F2(
									function (v0, v1) {
										return {ctor: '_Tuple2', _0: v0, _1: v1};
									}),
								'elm-mdl-header',
								A4(
									_debois$elm_mdl$Material_Layout$headerView,
									lift,
									config,
									model,
									{ctor: '_Tuple3', _0: headerDrawerButton, _1: _p36, _2: tabsElems}))) : _elm_lang$core$Maybe$Nothing,
						_1: {
							ctor: '::',
							_0: (!hasDrawer) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
								{
									ctor: '_Tuple2',
									_0: 'elm-mdl-drawer',
									_1: A3(_debois$elm_mdl$Material_Layout$drawerView, lift, drawerIsVisible, _p35)
								}),
							_1: {
								ctor: '::',
								_0: (!hasDrawer) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
									{
										ctor: '_Tuple2',
										_0: 'elm-mdl-obfuscator',
										_1: A2(_debois$elm_mdl$Material_Layout$obfuscator, lift, drawerIsVisible)
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$core$Maybe$map,
										F2(
											function (v0, v1) {
												return {ctor: '_Tuple2', _0: v0, _1: v1};
											})('elm-drawer-button'),
										contentDrawerButton),
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Maybe$Just(
											A2(
												F2(
													function (v0, v1) {
														return {ctor: '_Tuple2', _0: v0, _1: v1};
													}),
												_elm_lang$core$Basics$toString(config.selectedTab),
												A3(
													_debois$elm_mdl$Material_Options$styled,
													_elm_lang$html$Html$main_,
													{
														ctor: '::',
														_0: _debois$elm_mdl$Material_Options$id(_debois$elm_mdl$Material_Layout$mainId),
														_1: {
															ctor: '::',
															_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__content'),
															_1: {
																ctor: '::',
																_0: A2(
																	_debois$elm_mdl$Material_Options$when,
																	_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling) && config.fixedHeader,
																	A2(_debois$elm_mdl$Material_Options$css, 'overflow-y', 'visible')),
																_1: {
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_Options$when,
																		_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling) && config.fixedHeader,
																		A2(_debois$elm_mdl$Material_Options$css, 'overflow-x', 'visible')),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Options$when,
																			_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling) && config.fixedHeader,
																			A2(_debois$elm_mdl$Material_Options$css, 'overflow', 'visible')),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Options$when,
																				_debois$elm_mdl$Material_Layout$isWaterfall(config.mode),
																				function (_p33) {
																					return _debois$elm_mdl$Material_Options_Internal$attribute(
																						A2(_elm_lang$html$Html_Events$on, 'scroll', _p33));
																				}(
																					A2(
																						_elm_lang$core$Json_Decode$map,
																						function (_p34) {
																							return lift(
																								A2(_debois$elm_mdl$Material_Layout$ScrollPane, config.fixedHeader, _p34));
																						},
																						_debois$elm_dom$DOM$target(_debois$elm_dom$DOM$scrollTop)))),
																			_1: {ctor: '[]'}
																		}
																	}
																}
															}
														}
													},
													_p29.main))),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}),
				_1: {ctor: '[]'}
			});
	});
var _debois$elm_mdl$Material_Layout$render = A3(_debois$elm_mdl$Material_Component$render1, _debois$elm_mdl$Material_Layout$get, _debois$elm_mdl$Material_Layout$view, _debois$elm_mdl$Material_Component$LayoutMsg);

var _debois$elm_mdl$Material_Toggles$group = function (_p0) {
	return _debois$elm_mdl$Material_Options$attribute(
		_elm_lang$html$Html_Attributes$name(_p0));
};
var _debois$elm_mdl$Material_Toggles$value = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (b, options) {
				return _elm_lang$core$Native_Utils.update(
					options,
					{value: b});
			})(_p1));
};
var _debois$elm_mdl$Material_Toggles$disabled = _debois$elm_mdl$Material_Options_Internal$attribute(
	_elm_lang$html$Html_Attributes$disabled(true));
var _debois$elm_mdl$Material_Toggles$ripple = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Toggles$defaultConfig = {
	value: false,
	ripple: false,
	input: {ctor: '[]'},
	container: {ctor: '[]'}
};
var _debois$elm_mdl$Material_Toggles$defaultModel = {ripple: _debois$elm_mdl$Material_Ripple$model, isFocused: false};
var _debois$elm_mdl$Material_Toggles$_p2 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.toggles;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{toggles: x});
		}),
	_debois$elm_mdl$Material_Toggles$defaultModel);
var _debois$elm_mdl$Material_Toggles$get = _debois$elm_mdl$Material_Toggles$_p2._0;
var _debois$elm_mdl$Material_Toggles$set = _debois$elm_mdl$Material_Toggles$_p2._1;
var _debois$elm_mdl$Material_Toggles$Model = F2(
	function (a, b) {
		return {ripple: a, isFocused: b};
	});
var _debois$elm_mdl$Material_Toggles$Config = F4(
	function (a, b, c, d) {
		return {value: a, ripple: b, input: c, container: d};
	});
var _debois$elm_mdl$Material_Toggles$SetFocus = function (a) {
	return {ctor: 'SetFocus', _0: a};
};
var _debois$elm_mdl$Material_Toggles$Ripple = function (a) {
	return {ctor: 'Ripple', _0: a};
};
var _debois$elm_mdl$Material_Toggles$update = F2(
	function (action, model) {
		var _p3 = action;
		if (_p3.ctor === 'Ripple') {
			return A2(
				_debois$elm_mdl$Material_Helpers$map2nd,
				_elm_lang$core$Platform_Cmd$map(_debois$elm_mdl$Material_Toggles$Ripple),
				A2(
					_debois$elm_mdl$Material_Helpers$map1st,
					function (r) {
						return _elm_lang$core$Native_Utils.update(
							model,
							{ripple: r});
					},
					A2(_debois$elm_mdl$Material_Ripple$update, _p3._0, model.ripple)));
		} else {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{isFocused: _p3._0}),
				_1: _elm_lang$core$Platform_Cmd$none
			};
		}
	});
var _debois$elm_mdl$Material_Toggles$react = A4(
	_debois$elm_mdl$Material_Component$react,
	_debois$elm_mdl$Material_Toggles$get,
	_debois$elm_mdl$Material_Toggles$set,
	_debois$elm_mdl$Material_Component$TogglesMsg,
	_debois$elm_mdl$Material_Component$generalise(_debois$elm_mdl$Material_Toggles$update));
var _debois$elm_mdl$Material_Toggles$top = F5(
	function (lift, kind, model, summary, elems) {
		var cfg = summary.config;
		return A4(
			_debois$elm_mdl$Material_Options_Internal$applyContainer,
			summary,
			_elm_lang$html$Html$label,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs(
					A2(_elm_lang$core$Basics_ops['++'], 'mdl-', kind)),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs(
						A2(_elm_lang$core$Basics_ops['++'], 'mdl-js-', kind)),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$when,
							cfg.ripple,
							_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect')),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								cfg.ripple,
								_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect--ignore-events')),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$cs('is-upgraded'),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$when,
										cfg.value,
										_debois$elm_mdl$Material_Options$cs('is-checked')),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Options$when,
											model.isFocused,
											_debois$elm_mdl$Material_Options$cs('is-focused')),
										_1: {
											ctor: '::',
											_0: A3(
												_debois$elm_mdl$Material_Options_Internal$on1,
												'focus',
												lift,
												_debois$elm_mdl$Material_Toggles$SetFocus(true)),
											_1: {
												ctor: '::',
												_0: A3(
													_debois$elm_mdl$Material_Options_Internal$on1,
													'blur',
													lift,
													_debois$elm_mdl$Material_Toggles$SetFocus(false)),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options_Internal$attribute(
														_debois$elm_mdl$Material_Helpers$blurOn('mouseup')),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			},
			_elm_lang$core$List$concat(
				{
					ctor: '::',
					_0: elems,
					_1: {
						ctor: '::',
						_0: cfg.ripple ? {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$map,
								function (_p4) {
									return lift(
										_debois$elm_mdl$Material_Toggles$Ripple(_p4));
								},
								A2(
									_debois$elm_mdl$Material_Ripple$view,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('mdl-switch__ripple-container mdl-js-ripple-effect mdl-ripple--center'),
										_1: {ctor: '[]'}
									},
									model.ripple)),
							_1: {ctor: '[]'}
						} : {ctor: '[]'},
						_1: {ctor: '[]'}
					}
				}));
	});
var _debois$elm_mdl$Material_Toggles$viewCheckbox = F4(
	function (lift, model, config, elems) {
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Toggles$defaultConfig, config);
		return A5(
			_debois$elm_mdl$Material_Toggles$top,
			lift,
			'checkbox',
			model,
			summary,
			{
				ctor: '::',
				_0: A4(
					_debois$elm_mdl$Material_Options_Internal$applyInput,
					summary,
					_elm_lang$html$Html$input,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-checkbox__input'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options_Internal$attribute(
								_elm_lang$html$Html_Attributes$type_('checkbox')),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options_Internal$attribute(
									_elm_lang$html$Html_Attributes$checked(summary.config.value)),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('mdl-checkbox__label'),
							_1: {ctor: '[]'}
						},
						elems),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('mdl-checkbox__focus-helper'),
								_1: {ctor: '[]'}
							},
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('mdl-checkbox__box-outline'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$span,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('mdl-checkbox__tick-outline'),
											_1: {ctor: '[]'}
										},
										{ctor: '[]'}),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _debois$elm_mdl$Material_Toggles$checkbox = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Toggles$get, _debois$elm_mdl$Material_Toggles$viewCheckbox, _debois$elm_mdl$Material_Component$TogglesMsg);
var _debois$elm_mdl$Material_Toggles$viewSwitch = F4(
	function (lift, model, config, elems) {
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Toggles$defaultConfig, config);
		return A5(
			_debois$elm_mdl$Material_Toggles$top,
			lift,
			'switch',
			model,
			summary,
			{
				ctor: '::',
				_0: A4(
					_debois$elm_mdl$Material_Options_Internal$applyInput,
					summary,
					_elm_lang$html$Html$input,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-switch__input'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options_Internal$attribute(
								_elm_lang$html$Html_Attributes$type_('checkbox')),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options_Internal$attribute(
									_elm_lang$html$Html_Attributes$checked(summary.config.value)),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('mdl-switch__label'),
							_1: {ctor: '[]'}
						},
						elems),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('mdl-switch__track'),
								_1: {ctor: '[]'}
							},
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$div,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('mdl-switch__thumb'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$span,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('mdl-switch__focus-helper'),
											_1: {ctor: '[]'}
										},
										{ctor: '[]'}),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _debois$elm_mdl$Material_Toggles$switch = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Toggles$get, _debois$elm_mdl$Material_Toggles$viewSwitch, _debois$elm_mdl$Material_Component$TogglesMsg);
var _debois$elm_mdl$Material_Toggles$viewRadio = F4(
	function (lift, model, config, elems) {
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Toggles$defaultConfig, config);
		return A5(
			_debois$elm_mdl$Material_Toggles$top,
			lift,
			'radio',
			model,
			summary,
			{
				ctor: '::',
				_0: A4(
					_debois$elm_mdl$Material_Options_Internal$applyInput,
					summary,
					_elm_lang$html$Html$input,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-radio__button'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$attribute(
								_elm_lang$html$Html_Attributes$type_('radio')),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$attribute(
									_elm_lang$html$Html_Attributes$checked(summary.config.value)),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('mdl-radio__label'),
							_1: {ctor: '[]'}
						},
						elems),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('mdl-radio__outer-circle'),
								_1: {ctor: '[]'}
							},
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('mdl-radio__inner-circle'),
									_1: {ctor: '[]'}
								},
								{ctor: '[]'}),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _debois$elm_mdl$Material_Toggles$radio = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Toggles$get, _debois$elm_mdl$Material_Toggles$viewRadio, _debois$elm_mdl$Material_Component$TogglesMsg);

var _debois$elm_mdl$Material_Tooltip$element = function (elem) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		function (options) {
			return _elm_lang$core$Native_Utils.update(
				options,
				{elem: elem});
		});
};
var _debois$elm_mdl$Material_Tooltip$isTooltipClass = function (path) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		function ($class) {
			return A2(_elm_lang$core$String$contains, 'mdl-tooltip', $class) ? _elm_lang$core$Json_Decode$succeed(true) : _elm_lang$core$Json_Decode$succeed(false);
		},
		A2(_elm_lang$core$Json_Decode$at, path, _debois$elm_dom$DOM$className));
};
var _debois$elm_mdl$Material_Tooltip$sibling = function (d) {
	var valid = function (path) {
		return A2(
			_elm_lang$core$Json_Decode$andThen,
			function (res) {
				return res ? A2(_elm_lang$core$Json_Decode$at, path, d) : _elm_lang$core$Json_Decode$fail('');
			},
			_debois$elm_mdl$Material_Tooltip$isTooltipClass(path));
	};
	var createPath = function (depth) {
		var parents = A2(_elm_lang$core$List$repeat, depth, 'parentElement');
		return A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: 'target',
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$Basics_ops['++'],
				parents,
				{
					ctor: '::',
					_0: 'nextSibling',
					_1: {ctor: '[]'}
				}));
	};
	var paths = A2(
		_elm_lang$core$List$map,
		createPath,
		A2(_elm_lang$core$List$range, 0, 4));
	return _elm_lang$core$Json_Decode$oneOf(
		A2(_elm_lang$core$List$map, valid, paths));
};
var _debois$elm_mdl$Material_Tooltip$update = F2(
	function (action, model) {
		var _p0 = action;
		if (_p0.ctor === 'Enter') {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{isActive: true, domState: _p0._0}),
				_1: _elm_lang$core$Platform_Cmd$none
			};
		} else {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{isActive: false}),
				_1: _elm_lang$core$Platform_Cmd$none
			};
		}
	});
var _debois$elm_mdl$Material_Tooltip$calculatePos = F2(
	function (pos, domState) {
		var getValuesFor = F2(
			function (l, r) {
				return (_elm_lang$core$Native_Utils.cmp(l + r, 0) < 0) ? {ctor: '_Tuple2', _0: 0, _1: 0} : {ctor: '_Tuple2', _0: l, _1: r};
			});
		var offsetHeight = domState.offsetHeight;
		var marginTop = -1 * (offsetHeight / 2);
		var offsetWidth = domState.offsetWidth;
		var marginLeft = -1 * (offsetWidth / 2);
		var props = domState.rect;
		var left = props.left + (props.width / 2);
		var _p1 = A2(getValuesFor, left, marginLeft);
		var newLeft = _p1._0;
		var newMarginLeft = _p1._1;
		var top = props.top + (props.height / 2);
		var _p2 = A2(getValuesFor, top, marginTop);
		var newTop = _p2._0;
		var newMarginTop = _p2._1;
		var out = function () {
			var _p3 = pos;
			switch (_p3.ctor) {
				case 'Left':
					return {left: (props.left - offsetWidth) - 10, top: newTop, marginTop: newMarginTop, marginLeft: 0};
				case 'Right':
					return {left: (props.left + props.width) + 10, top: newTop, marginTop: newMarginTop, marginLeft: 0};
				case 'Top':
					return {left: newLeft, top: (props.top - offsetHeight) - 10, marginTop: 0, marginLeft: newMarginLeft};
				default:
					return {left: newLeft, top: (props.top + props.height) + 10, marginTop: 0, marginLeft: newMarginLeft};
			}
		}();
		return out;
	});
var _debois$elm_mdl$Material_Tooltip$defaultDOMState = {
	rect: {left: 0, top: 0, width: 0, height: 0},
	offsetWidth: 0,
	offsetHeight: 0
};
var _debois$elm_mdl$Material_Tooltip$defaultPos = {left: 0, top: 0, marginLeft: 0, marginTop: 0};
var _debois$elm_mdl$Material_Tooltip$defaultModel = {isActive: false, domState: _debois$elm_mdl$Material_Tooltip$defaultDOMState};
var _debois$elm_mdl$Material_Tooltip$_p4 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.tooltip;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{tooltip: x});
		}),
	_debois$elm_mdl$Material_Tooltip$defaultModel);
var _debois$elm_mdl$Material_Tooltip$get = _debois$elm_mdl$Material_Tooltip$_p4._0;
var _debois$elm_mdl$Material_Tooltip$set = _debois$elm_mdl$Material_Tooltip$_p4._1;
var _debois$elm_mdl$Material_Tooltip$react = A4(
	_debois$elm_mdl$Material_Component$react,
	_debois$elm_mdl$Material_Tooltip$get,
	_debois$elm_mdl$Material_Tooltip$set,
	_debois$elm_mdl$Material_Component$TooltipMsg,
	_debois$elm_mdl$Material_Component$generalise(_debois$elm_mdl$Material_Tooltip$update));
var _debois$elm_mdl$Material_Tooltip$Model = F2(
	function (a, b) {
		return {isActive: a, domState: b};
	});
var _debois$elm_mdl$Material_Tooltip$Pos = F4(
	function (a, b, c, d) {
		return {left: a, top: b, marginLeft: c, marginTop: d};
	});
var _debois$elm_mdl$Material_Tooltip$DOMState = F3(
	function (a, b, c) {
		return {rect: a, offsetWidth: b, offsetHeight: c};
	});
var _debois$elm_mdl$Material_Tooltip$stateDecoder = A4(
	_elm_lang$core$Json_Decode$map3,
	_debois$elm_mdl$Material_Tooltip$DOMState,
	_debois$elm_dom$DOM$target(_debois$elm_dom$DOM$boundingClientRect),
	_debois$elm_mdl$Material_Tooltip$sibling(_debois$elm_dom$DOM$offsetWidth),
	_debois$elm_mdl$Material_Tooltip$sibling(_debois$elm_dom$DOM$offsetHeight));
var _debois$elm_mdl$Material_Tooltip$Config = F3(
	function (a, b, c) {
		return {size: a, position: b, elem: c};
	});
var _debois$elm_mdl$Material_Tooltip$Leave = {ctor: 'Leave'};
var _debois$elm_mdl$Material_Tooltip$onMouseLeave = F2(
	function (lift, idx) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseleave',
			_elm_lang$core$Json_Decode$succeed(
				lift(
					A2(_debois$elm_mdl$Material_Component$TooltipMsg, idx, _debois$elm_mdl$Material_Tooltip$Leave))));
	});
var _debois$elm_mdl$Material_Tooltip$onLeave = function (lift) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		A2(
			_elm_lang$core$Json_Decode$map,
			lift,
			_elm_lang$core$Json_Decode$succeed(_debois$elm_mdl$Material_Tooltip$Leave)));
};
var _debois$elm_mdl$Material_Tooltip$Enter = function (a) {
	return {ctor: 'Enter', _0: a};
};
var _debois$elm_mdl$Material_Tooltip$onMouseEnter = F2(
	function (lift, idx) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseenter',
			A2(
				_elm_lang$core$Json_Decode$map,
				function (_p5) {
					return lift(
						A2(
							_debois$elm_mdl$Material_Component$TooltipMsg,
							idx,
							_debois$elm_mdl$Material_Tooltip$Enter(_p5)));
				},
				_debois$elm_mdl$Material_Tooltip$stateDecoder));
	});
var _debois$elm_mdl$Material_Tooltip$attach = F2(
	function (lift, index) {
		return _debois$elm_mdl$Material_Options$many(
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options_Internal$attribute(
					A2(_debois$elm_mdl$Material_Tooltip$onMouseEnter, lift, index)),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options_Internal$attribute(
						A2(_debois$elm_mdl$Material_Tooltip$onMouseLeave, lift, index)),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_mdl$Material_Tooltip$onEnter = function (lift) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		A2(
			_elm_lang$core$Json_Decode$map,
			lift,
			A2(_elm_lang$core$Json_Decode$map, _debois$elm_mdl$Material_Tooltip$Enter, _debois$elm_mdl$Material_Tooltip$stateDecoder)));
};
var _debois$elm_mdl$Material_Tooltip$Large = {ctor: 'Large'};
var _debois$elm_mdl$Material_Tooltip$large = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{size: _debois$elm_mdl$Material_Tooltip$Large});
	});
var _debois$elm_mdl$Material_Tooltip$Default = {ctor: 'Default'};
var _debois$elm_mdl$Material_Tooltip$Bottom = {ctor: 'Bottom'};
var _debois$elm_mdl$Material_Tooltip$defaultConfig = {size: _debois$elm_mdl$Material_Tooltip$Default, position: _debois$elm_mdl$Material_Tooltip$Bottom, elem: _elm_lang$html$Html$div};
var _debois$elm_mdl$Material_Tooltip$view = F4(
	function (lift, model, options, content) {
		var px = function (f) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(f),
				'px');
		};
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Tooltip$defaultConfig, options);
		var config = summary.config;
		var pos = model.isActive ? A2(_debois$elm_mdl$Material_Tooltip$calculatePos, config.position, model.domState) : _debois$elm_mdl$Material_Tooltip$defaultPos;
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			config.elem,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-tooltip'),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$when,
						model.isActive,
						_debois$elm_mdl$Material_Options$cs('is-active')),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$when,
							_elm_lang$core$Native_Utils.eq(config.size, _debois$elm_mdl$Material_Tooltip$Large),
							_debois$elm_mdl$Material_Options$cs('mdl-tooltip--large')),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								model.isActive,
								A2(
									_debois$elm_mdl$Material_Options$css,
									'left',
									px(pos.left))),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Options$when,
									model.isActive,
									A2(
										_debois$elm_mdl$Material_Options$css,
										'margin-left',
										px(pos.marginLeft))),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$when,
										model.isActive,
										A2(
											_debois$elm_mdl$Material_Options$css,
											'top',
											px(pos.top))),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Options$when,
											model.isActive,
											A2(
												_debois$elm_mdl$Material_Options$css,
												'margin-top',
												px(pos.marginTop))),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			},
			content);
	});
var _debois$elm_mdl$Material_Tooltip$render = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Tooltip$get, _debois$elm_mdl$Material_Tooltip$view, _debois$elm_mdl$Material_Component$TooltipMsg);
var _debois$elm_mdl$Material_Tooltip$bottom = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Bottom});
	});
var _debois$elm_mdl$Material_Tooltip$Top = {ctor: 'Top'};
var _debois$elm_mdl$Material_Tooltip$top = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Top});
	});
var _debois$elm_mdl$Material_Tooltip$Right = {ctor: 'Right'};
var _debois$elm_mdl$Material_Tooltip$right = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Right});
	});
var _debois$elm_mdl$Material_Tooltip$Left = {ctor: 'Left'};
var _debois$elm_mdl$Material_Tooltip$left = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Left});
	});

var _debois$elm_mdl$Material_Tabs$activeTab = function (_p0) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (k, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{activeTab: k});
			})(_p0));
};
var _debois$elm_mdl$Material_Tabs$onSelectTab = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (k, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						onSelectTab: _elm_lang$core$Maybe$Just(k)
					});
			})(_p1));
};
var _debois$elm_mdl$Material_Tabs$ripple = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Tabs$defaultConfig = {ripple: false, onSelectTab: _elm_lang$core$Maybe$Nothing, activeTab: 0};
var _debois$elm_mdl$Material_Tabs$defaultModel = {ripples: _elm_lang$core$Dict$empty};
var _debois$elm_mdl$Material_Tabs$_p2 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.tabs;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{tabs: x});
		}),
	_debois$elm_mdl$Material_Tabs$defaultModel);
var _debois$elm_mdl$Material_Tabs$get = _debois$elm_mdl$Material_Tabs$_p2._0;
var _debois$elm_mdl$Material_Tabs$set = _debois$elm_mdl$Material_Tabs$_p2._1;
var _debois$elm_mdl$Material_Tabs$Model = function (a) {
	return {ripples: a};
};
var _debois$elm_mdl$Material_Tabs$Config = F3(
	function (a, b, c) {
		return {ripple: a, onSelectTab: b, activeTab: c};
	});
var _debois$elm_mdl$Material_Tabs$Ripple = F2(
	function (a, b) {
		return {ctor: 'Ripple', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Tabs$update = F2(
	function (action, model) {
		var _p3 = action;
		var _p5 = _p3._0;
		var _p4 = A2(
			_debois$elm_mdl$Material_Ripple$update,
			_p3._1,
			A2(
				_elm_lang$core$Maybe$withDefault,
				_debois$elm_mdl$Material_Ripple$model,
				A2(_elm_lang$core$Dict$get, _p5, model.ripples)));
		var ripple_ = _p4._0;
		var cmd = _p4._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				model,
				{
					ripples: A3(_elm_lang$core$Dict$insert, _p5, ripple_, model.ripples)
				}),
			_1: A2(
				_elm_lang$core$Platform_Cmd$map,
				_debois$elm_mdl$Material_Tabs$Ripple(_p5),
				cmd)
		};
	});
var _debois$elm_mdl$Material_Tabs$react = A4(
	_debois$elm_mdl$Material_Component$react,
	_debois$elm_mdl$Material_Tabs$get,
	_debois$elm_mdl$Material_Tabs$set,
	_debois$elm_mdl$Material_Component$TabsMsg,
	_debois$elm_mdl$Material_Component$generalise(_debois$elm_mdl$Material_Tabs$update));
var _debois$elm_mdl$Material_Tabs$view = F5(
	function (lift, model, options, tabs, tabContent) {
		var wrapContent = A2(
			_elm_lang$html$Html_Keyed$node,
			'div',
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'mdl-tab__panel', _1: true},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'is-active', _1: true},
							_1: {ctor: '[]'}
						}
					}),
				_1: {ctor: '[]'}
			});
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Tabs$defaultConfig, options);
		var config = summary.config;
		var unwrapLabel = F2(
			function (tabIdx, _p6) {
				var _p7 = _p6;
				var _p9 = _p7._0._1;
				return A3(
					_debois$elm_mdl$Material_Options$styled,
					_elm_lang$html$Html$a,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-tabs__tab'),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								_elm_lang$core$Native_Utils.eq(tabIdx, config.activeTab),
								_debois$elm_mdl$Material_Options$cs('is-active')),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$core$Maybe$withDefault,
									_debois$elm_mdl$Material_Options$nop,
									A2(
										_elm_lang$core$Maybe$map,
										function (t) {
											return _debois$elm_mdl$Material_Options$onClick(
												t(tabIdx));
										},
										config.onSelectTab)),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$many(_p7._0._0),
									_1: {ctor: '[]'}
								}
							}
						}
					},
					config.ripple ? _elm_lang$core$List$concat(
						{
							ctor: '::',
							_0: _p9,
							_1: {
								ctor: '::',
								_0: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$map,
										function (_p8) {
											return lift(
												A2(_debois$elm_mdl$Material_Tabs$Ripple, tabIdx, _p8));
										},
										A2(
											_debois$elm_mdl$Material_Ripple$view,
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$classList(
													{
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'mdl-tabs__ripple-container', _1: true},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'mdl-tabs__ripple-js-effect', _1: true},
															_1: {ctor: '[]'}
														}
													}),
												_1: {ctor: '[]'}
											},
											A2(
												_elm_lang$core$Maybe$withDefault,
												_debois$elm_mdl$Material_Ripple$model,
												A2(_elm_lang$core$Dict$get, tabIdx, model.ripples)))),
									_1: {ctor: '[]'}
								},
								_1: {ctor: '[]'}
							}
						}) : _p9);
			});
		var links = A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-tabs__tab-bar'),
				_1: {ctor: '[]'}
			},
			A2(_elm_lang$core$List$indexedMap, unwrapLabel, tabs));
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-tabs'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-js-tabs'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('is-upgraded'),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								config.ripple,
								_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect')),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Options$when,
									config.ripple,
									_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect--ignore-events')),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			},
			{ctor: '[]'},
			{
				ctor: '::',
				_0: links,
				_1: {
					ctor: '::',
					_0: wrapContent(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Basics$toString(config.activeTab),
								_1: A2(
									_elm_lang$html$Html$div,
									{ctor: '[]'},
									tabContent)
							},
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_mdl$Material_Tabs$render = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Tabs$get, _debois$elm_mdl$Material_Tabs$view, _debois$elm_mdl$Material_Component$TabsMsg);
var _debois$elm_mdl$Material_Tabs$Label = function (a) {
	return {ctor: 'Label', _0: a};
};
var _debois$elm_mdl$Material_Tabs$label = F2(
	function (p, c) {
		return _debois$elm_mdl$Material_Tabs$Label(
			{ctor: '_Tuple2', _0: p, _1: c});
	});
var _debois$elm_mdl$Material_Tabs$textLabel = F2(
	function (p, c) {
		return A2(
			_debois$elm_mdl$Material_Tabs$label,
			p,
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(c),
				_1: {ctor: '[]'}
			});
	});

var _debois$elm_mdl$Material$init = function (lift) {
	return _debois$elm_mdl$Material_Layout$sub0(lift);
};
var _debois$elm_mdl$Material$subscriptions = F2(
	function (lift, model) {
		return _elm_lang$core$Platform_Sub$batch(
			{
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Layout$subs, lift, model.mdl),
				_1: {
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Menu$subs, lift, model.mdl),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_mdl$Material$update_ = F3(
	function (lift, msg, store) {
		var _p0 = msg;
		switch (_p0.ctor) {
			case 'ButtonMsg':
				return A4(_debois$elm_mdl$Material_Button$react, lift, _p0._1, _p0._0, store);
			case 'TextfieldMsg':
				return A4(_debois$elm_mdl$Material_Textfield$react, lift, _p0._1, _p0._0, store);
			case 'MenuMsg':
				var _p2 = _p0._0;
				return A4(
					_debois$elm_mdl$Material_Menu$react,
					function (_p1) {
						return lift(
							A2(_debois$elm_mdl$Material_Component$MenuMsg, _p2, _p1));
					},
					_p0._1,
					_p2,
					store);
			case 'LayoutMsg':
				return A3(
					_debois$elm_mdl$Material_Layout$react,
					function (_p3) {
						return lift(
							_debois$elm_mdl$Material_Component$LayoutMsg(_p3));
					},
					_p0._0,
					store);
			case 'TogglesMsg':
				return A4(_debois$elm_mdl$Material_Toggles$react, lift, _p0._1, _p0._0, store);
			case 'TooltipMsg':
				return A4(_debois$elm_mdl$Material_Tooltip$react, lift, _p0._1, _p0._0, store);
			case 'TabsMsg':
				return A4(_debois$elm_mdl$Material_Tabs$react, lift, _p0._1, _p0._0, store);
			default:
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Maybe$Nothing,
					_1: _debois$elm_mdl$Material_Dispatch$forward(_p0._0)
				};
		}
	});
var _debois$elm_mdl$Material$update = F3(
	function (lift, msg, container) {
		return A2(
			_debois$elm_mdl$Material_Helpers$map1st,
			_elm_lang$core$Maybe$withDefault(container),
			A2(
				_debois$elm_mdl$Material_Helpers$map1st,
				_elm_lang$core$Maybe$map(
					function (mdl) {
						return _elm_lang$core$Native_Utils.update(
							container,
							{mdl: mdl});
					}),
				A3(
					_debois$elm_mdl$Material$update_,
					lift,
					msg,
					function (_) {
						return _.mdl;
					}(container))));
	});
var _debois$elm_mdl$Material$model = {button: _elm_lang$core$Dict$empty, textfield: _elm_lang$core$Dict$empty, menu: _elm_lang$core$Dict$empty, snackbar: _elm_lang$core$Maybe$Nothing, layout: _debois$elm_mdl$Material_Layout$defaultModel, toggles: _elm_lang$core$Dict$empty, tooltip: _elm_lang$core$Dict$empty, tabs: _elm_lang$core$Dict$empty};
var _debois$elm_mdl$Material$Model = F8(
	function (a, b, c, d, e, f, g, h) {
		return {button: a, textfield: b, menu: c, snackbar: d, layout: e, toggles: f, tooltip: g, tabs: h};
	});

var _debois$elm_mdl$Material_Dialog$theDialog = 'elm-mdl-singleton-dialog';
var _debois$elm_mdl$Material_Dialog$openOn = function () {
	var handler = A2(
		_elm_lang$core$Basics_ops['++'],
		'\n      // Don\'t mess up the elm runtime.\n      try {\n        var dialog = document.getElementById(\'',
		A2(_elm_lang$core$Basics_ops['++'], _debois$elm_mdl$Material_Dialog$theDialog, '\');\n        if (! dialog) {\n          console.log (\'Cannot display dialog: No dialog element. Use `Dialog.view` to construct one.\');\n          return;\n        }\n        if (! dialog.showModal) {\n          if (typeof dialogPolyfill !== \'undefined\' && dialogPolyfill.registerDialog) {\n            dialogPolyfill.registerDialog(dialog);\n          } else {\n            console.log (\'Cannot display dialog: Your browser does not support the <dialog> element. Get a polyfill at:\\n\\nhttps://github.com/GoogleChrome/dialog-polyfill\\n\');\n            return;\n          }\n        }\n        dialog.showModal();\n      }\n      catch (e)\n      {\n        console.log (\"A dialog method threw an exception. This is not supposed to happen; likely you\'re using a broken polyfill. If not, please file an issue:\\n\\nhttps://github.com/debois/elm-mdl/issues/new\");\n      }\n      '));
	return function (event) {
		return _debois$elm_mdl$Material_Options_Internal$attribute(
			A2(
				_elm_lang$html$Html_Attributes$attribute,
				A2(_elm_lang$core$Basics_ops['++'], 'on', event),
				handler));
	};
}();
var _debois$elm_mdl$Material_Dialog$closeOn = function () {
	var handler = A2(
		_elm_lang$core$Basics_ops['++'],
		'\n      // Don\'t mess up the elm runtime!\n      try {\n        var dialog = document.getElementById(\'',
		A2(_elm_lang$core$Basics_ops['++'], _debois$elm_mdl$Material_Dialog$theDialog, '\');\n        if (! dialog) {\n          console.log (\'Cannot close dialog: No dialog element. Use `Dialog.view` to construct one.\');\n          return;\n        }\n        if (! dialog.open) {\n          console.log (\'Cannot close dialog: The dialog is not open. Use `Dialog.closeOn` only on components rendered inside the dialog.\');\n          return;\n        }\n        if (! dialog.close) {\n          console.log (\'Cannot close dialog: The dialog does not have a `close` method. Perhaps you forgot a polyfill? Get one at:\\n\\nhttps://github.com/GoogleChrome/dialog-polyfill\\n\');\n          return;\n        }\n        dialog.close();\n      }\n      catch (e)\n      {\n        console.log (\"A dialog method threw an exception. This is not supposed to happen; likely you\'re using a broken polyfill. If not, please file an issue:\\n\\nhttps://github.com/debois/elm-mdl/issues/new\");\n      }\n      '));
	return function (event) {
		return _debois$elm_mdl$Material_Options_Internal$attribute(
			A2(
				_elm_lang$html$Html_Attributes$attribute,
				A2(_elm_lang$core$Basics_ops['++'], 'on', event),
				handler));
	};
}();
var _debois$elm_mdl$Material_Dialog$contentBlock = function (block) {
	var _p0 = block;
	switch (_p0.ctor) {
		case 'Title':
			return A2(
				_debois$elm_mdl$Material_Options$div,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-dialog__title'),
					_1: _p0._0
				},
				_p0._1);
		case 'Content':
			return A2(
				_debois$elm_mdl$Material_Options$div,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-dialog__content'),
					_1: _p0._0
				},
				_p0._1);
		default:
			return A2(
				_debois$elm_mdl$Material_Options$div,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-dialog__actions'),
					_1: _p0._0
				},
				_p0._1);
	}
};
var _debois$elm_mdl$Material_Dialog$view = F2(
	function (styling, contentBlocks) {
		return A4(
			_debois$elm_mdl$Material_Options$styled_,
			_elm_lang$html$Html$node('dialog'),
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-dialog'),
				_1: styling
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$id(_debois$elm_mdl$Material_Dialog$theDialog),
				_1: {ctor: '[]'}
			},
			A2(_elm_lang$core$List$map, _debois$elm_mdl$Material_Dialog$contentBlock, contentBlocks));
	});
var _debois$elm_mdl$Material_Dialog$fullWidth = _debois$elm_mdl$Material_Options$cs('mdl-dialog__actions--full-width');
var _debois$elm_mdl$Material_Dialog$Actions = F2(
	function (a, b) {
		return {ctor: 'Actions', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Dialog$actions = _debois$elm_mdl$Material_Dialog$Actions;
var _debois$elm_mdl$Material_Dialog$Content = F2(
	function (a, b) {
		return {ctor: 'Content', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Dialog$content = _debois$elm_mdl$Material_Dialog$Content;
var _debois$elm_mdl$Material_Dialog$Title = F2(
	function (a, b) {
		return {ctor: 'Title', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Dialog$title = _debois$elm_mdl$Material_Dialog$Title;

var _debois$elm_mdl$Material_Grid$clip = F3(
	function (lower, upper, k) {
		return A2(
			_elm_lang$core$Basics$max,
			lower,
			A2(_elm_lang$core$Basics$min, k, upper));
	});
var _debois$elm_mdl$Material_Grid$stretch = _debois$elm_mdl$Material_Options$cs('mdl-cell--stretch');
var _debois$elm_mdl$Material_Grid$align = function (a) {
	var _p0 = a;
	switch (_p0.ctor) {
		case 'Top':
			return _debois$elm_mdl$Material_Options$cs('mdl-cell--top');
		case 'Middle':
			return _debois$elm_mdl$Material_Options$cs('mdl-cell--middle');
		default:
			return _debois$elm_mdl$Material_Options$cs('mdl-cell--bottom');
	}
};
var _debois$elm_mdl$Material_Grid$suffix = function (device) {
	var _p1 = device;
	switch (_p1.ctor) {
		case 'All':
			return '';
		case 'Desktop':
			return '-desktop';
		case 'Tablet':
			return '-tablet';
		default:
			return '-phone';
	}
};
var _debois$elm_mdl$Material_Grid$size = F2(
	function (device, k) {
		var c = function () {
			var _p2 = device;
			switch (_p2.ctor) {
				case 'All':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 12, k);
				case 'Desktop':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 12, k);
				case 'Tablet':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 8, k);
				default:
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 4, k);
			}
		}();
		return _debois$elm_mdl$Material_Options$cs(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'mdl-cell--',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(c),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'-col',
						_debois$elm_mdl$Material_Grid$suffix(device)))));
	});
var _debois$elm_mdl$Material_Grid$offset = F2(
	function (device, k) {
		var c = function () {
			var _p3 = device;
			switch (_p3.ctor) {
				case 'All':
					return A3(_debois$elm_mdl$Material_Grid$clip, 0, 11, k);
				case 'Desktop':
					return A3(_debois$elm_mdl$Material_Grid$clip, 0, 11, k);
				case 'Tablet':
					return A3(_debois$elm_mdl$Material_Grid$clip, 0, 7, k);
				default:
					return A3(_debois$elm_mdl$Material_Grid$clip, 0, 3, k);
			}
		}();
		return _debois$elm_mdl$Material_Options$cs(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'mdl-cell--',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(c),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'-offset',
						_debois$elm_mdl$Material_Grid$suffix(device)))));
	});
var _debois$elm_mdl$Material_Grid$hide = function (device) {
	return _debois$elm_mdl$Material_Options$cs(
		function () {
			var _p4 = device;
			if (_p4.ctor === 'All') {
				return '';
			} else {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'mdl-cell--hide-',
					_debois$elm_mdl$Material_Grid$suffix(device));
			}
		}());
};
var _debois$elm_mdl$Material_Grid$order = F2(
	function (device, n) {
		return _debois$elm_mdl$Material_Options$cs(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'mdl-cell--order-',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(
						A3(_debois$elm_mdl$Material_Grid$clip, 1, 12, n)),
					_debois$elm_mdl$Material_Grid$suffix(device))));
	});
var _debois$elm_mdl$Material_Grid$grid = F2(
	function (styling, cells) {
		return A2(
			_debois$elm_mdl$Material_Options$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-grid'),
				_1: styling
			},
			A2(
				_elm_lang$core$List$map,
				function (_p5) {
					var _p6 = _p5;
					return _p6._0;
				},
				cells));
	});
var _debois$elm_mdl$Material_Grid$maxWidth = function (w) {
	return A2(_debois$elm_mdl$Material_Options$css, 'max-width', w);
};
var _debois$elm_mdl$Material_Grid$noSpacing = _debois$elm_mdl$Material_Options$cs('mdl-grid--no-spacing');
var _debois$elm_mdl$Material_Grid$Phone = {ctor: 'Phone'};
var _debois$elm_mdl$Material_Grid$Tablet = {ctor: 'Tablet'};
var _debois$elm_mdl$Material_Grid$Desktop = {ctor: 'Desktop'};
var _debois$elm_mdl$Material_Grid$All = {ctor: 'All'};
var _debois$elm_mdl$Material_Grid$Cell = function (a) {
	return {ctor: 'Cell', _0: a};
};
var _debois$elm_mdl$Material_Grid$cell = F2(
	function (styling, elms) {
		return _debois$elm_mdl$Material_Grid$Cell(
			A2(
				_debois$elm_mdl$Material_Options$div,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-cell'),
					_1: styling
				},
				elms));
	});
var _debois$elm_mdl$Material_Grid$Bottom = {ctor: 'Bottom'};
var _debois$elm_mdl$Material_Grid$Middle = {ctor: 'Middle'};
var _debois$elm_mdl$Material_Grid$Top = {ctor: 'Top'};

var _debois$elm_mdl$Material_List$action2 = _debois$elm_mdl$Material_Options$cs('mdl-list__item-secondary-action');
var _debois$elm_mdl$Material_List$info2 = function (options) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-secondary-info'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$content2 = function (options) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-secondary-content'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$subtitle = function (options) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-sub-title'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$body = function (options) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-text-body'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$icon = F2(
	function (i, options) {
		return A2(
			_debois$elm_mdl$Material_Icon$view,
			i,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-icon'),
				_1: options
			});
	});
var _debois$elm_mdl$Material_List$avatar = _debois$elm_mdl$Material_Options$cs('mdl-list__item-avatar');
var _debois$elm_mdl$Material_List$avatarImage = F2(
	function (src, options) {
		return A4(
			_debois$elm_mdl$Material_Options$styled_,
			_elm_lang$html$Html$img,
			{ctor: '::', _0: _debois$elm_mdl$Material_List$avatar, _1: options},
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$src(src),
				_1: {ctor: '[]'}
			},
			{ctor: '[]'});
	});
var _debois$elm_mdl$Material_List$avatarIcon = F2(
	function (i, options) {
		return A2(
			_debois$elm_mdl$Material_Options$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$center,
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$many(options),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_List$avatar,
						_1: {ctor: '[]'}
					}
				}
			},
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Icon$i(i),
				_1: {ctor: '[]'}
			});
	});
var _debois$elm_mdl$Material_List$content = function (options) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-primary-content'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$withSubtitle = _debois$elm_mdl$Material_Options$cs('mdl-list__item--two-line');
var _debois$elm_mdl$Material_List$withBody = _debois$elm_mdl$Material_Options$cs('mdl-list__item--three-line');
var _debois$elm_mdl$Material_List$li = function (options) {
	return A2(
		_debois$elm_mdl$Material_Options$styled,
		_elm_lang$html$Html$li,
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$ul = function (options) {
	return A2(
		_debois$elm_mdl$Material_Options$styled,
		_elm_lang$html$Html$ul,
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list'),
			_1: options
		});
};

var _debois$elm_mdl$Material_Spinner$layer = function (n) {
	return A2(
		_debois$elm_mdl$Material_Options$div,
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'mdl-spinner__layer mdl-spinner__layer-',
					_elm_lang$core$Basics$toString(n))),
			_1: {ctor: '[]'}
		},
		A2(
			_elm_lang$core$List$map,
			F2(
				function (x, y) {
					return y(x);
				})(
				{
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$div,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('mdl-spinner__circle'),
							_1: {ctor: '[]'}
						},
						{ctor: '[]'}),
					_1: {ctor: '[]'}
				}),
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$div(
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-spinner__circle-clipper mdl-spinner__left'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$div(
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('mdl-spinner__gap-patch'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$div(
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$cs('mdl-spinner__circle-clipper mdl-spinner__right'),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				}
			}));
};
var _debois$elm_mdl$Material_Spinner$layers = A2(
	_elm_lang$core$List$map,
	_debois$elm_mdl$Material_Spinner$layer,
	A2(_elm_lang$core$List$range, 1, 4));
var _debois$elm_mdl$Material_Spinner$defaultConfig = {active: false, singleColor: false};
var _debois$elm_mdl$Material_Spinner$singleColor = function (_p0) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (value, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{singleColor: value});
			})(_p0));
};
var _debois$elm_mdl$Material_Spinner$active = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (value, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{active: value});
			})(_p1));
};
var _debois$elm_mdl$Material_Spinner$spinner = function (options) {
	var _p2 = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Spinner$defaultConfig, options);
	var summary = _p2;
	var config = _p2.config;
	return A5(
		_debois$elm_mdl$Material_Options_Internal$apply,
		summary,
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-spinner mdl-js-spinner is-upgraded'),
			_1: {
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Options$when,
					config.active,
					_debois$elm_mdl$Material_Options$cs('is-active')),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$when,
						config.singleColor,
						_debois$elm_mdl$Material_Options$cs('mdl-spinner--single-color')),
					_1: {ctor: '[]'}
				}
			}
		},
		{ctor: '[]'},
		_debois$elm_mdl$Material_Spinner$layers);
};
var _debois$elm_mdl$Material_Spinner$Config = F2(
	function (a, b) {
		return {active: a, singleColor: b};
	});

var _debois$elm_mdl$Material_Typography$uppercase = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-uppercase');
var _debois$elm_mdl$Material_Typography$lowercase = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-lowercase');
var _debois$elm_mdl$Material_Typography$capitalize = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-capitalize');
var _debois$elm_mdl$Material_Typography$justify = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-justify');
var _debois$elm_mdl$Material_Typography$right = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-right');
var _debois$elm_mdl$Material_Typography$left = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-left');
var _debois$elm_mdl$Material_Typography$center = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-center');
var _debois$elm_mdl$Material_Typography$tableStriped = _debois$elm_mdl$Material_Options$cs('mdl-typography--table-striped');
var _debois$elm_mdl$Material_Typography$nowrap = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-nowrap');
var _debois$elm_mdl$Material_Typography$contrast = function (x) {
	return A2(
		_debois$elm_mdl$Material_Options$css,
		'opacity',
		_elm_lang$core$Basics$toString(x));
};
var _debois$elm_mdl$Material_Typography$menu = _debois$elm_mdl$Material_Options$cs('mdl-typography--menu-color-contrast');
var _debois$elm_mdl$Material_Typography$button = _debois$elm_mdl$Material_Options$cs('mdl-typography--button-color-contrast');
var _debois$elm_mdl$Material_Typography$caption = _debois$elm_mdl$Material_Options$cs('mdl-typography--caption-force-preferred-font-color-contrast');
var _debois$elm_mdl$Material_Typography$body2 = _debois$elm_mdl$Material_Options$cs('mdl-typography--body-2-force-preferred-font-color-contrast');
var _debois$elm_mdl$Material_Typography$body1 = _debois$elm_mdl$Material_Options$cs('mdl-typography--body-1-force-preferred-font-color-contrast');
var _debois$elm_mdl$Material_Typography$subhead = _debois$elm_mdl$Material_Options$cs('mdl-typography--subhead-color-contrast');
var _debois$elm_mdl$Material_Typography$title = _debois$elm_mdl$Material_Options$cs('mdl-typography--title-color-contrast');
var _debois$elm_mdl$Material_Typography$headline = _debois$elm_mdl$Material_Options$cs('mdl-typography--headline-color-contrast');
var _debois$elm_mdl$Material_Typography$display4 = _debois$elm_mdl$Material_Options$cs('mdl-typography--display-4-color-contrast');
var _debois$elm_mdl$Material_Typography$display3 = _debois$elm_mdl$Material_Options$cs('mdl-typography--display-3-color-contrast');
var _debois$elm_mdl$Material_Typography$display2 = _debois$elm_mdl$Material_Options$cs('mdl-typography--display-2-color-contrast');
var _debois$elm_mdl$Material_Typography$display1 = _debois$elm_mdl$Material_Options$cs('mdl-typography--display-1-color-contrast');

var _elm_lang$http$Native_Http = function() {


// ENCODING AND DECODING

function encodeUri(string)
{
	return encodeURIComponent(string);
}

function decodeUri(string)
{
	try
	{
		return _elm_lang$core$Maybe$Just(decodeURIComponent(string));
	}
	catch(e)
	{
		return _elm_lang$core$Maybe$Nothing;
	}
}


// SEND REQUEST

function toTask(request, maybeProgress)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var xhr = new XMLHttpRequest();

		configureProgress(xhr, maybeProgress);

		xhr.addEventListener('error', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NetworkError' }));
		});
		xhr.addEventListener('timeout', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'Timeout' }));
		});
		xhr.addEventListener('load', function() {
			callback(handleResponse(xhr, request.expect.responseToResult));
		});

		try
		{
			xhr.open(request.method, request.url, true);
		}
		catch (e)
		{
			return callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'BadUrl', _0: request.url }));
		}

		configureRequest(xhr, request);
		send(xhr, request.body);

		return function() { xhr.abort(); };
	});
}

function configureProgress(xhr, maybeProgress)
{
	if (maybeProgress.ctor === 'Nothing')
	{
		return;
	}

	xhr.addEventListener('progress', function(event) {
		if (!event.lengthComputable)
		{
			return;
		}
		_elm_lang$core$Native_Scheduler.rawSpawn(maybeProgress._0({
			bytes: event.loaded,
			bytesExpected: event.total
		}));
	});
}

function configureRequest(xhr, request)
{
	function setHeader(pair)
	{
		xhr.setRequestHeader(pair._0, pair._1);
	}

	A2(_elm_lang$core$List$map, setHeader, request.headers);
	xhr.responseType = request.expect.responseType;
	xhr.withCredentials = request.withCredentials;

	if (request.timeout.ctor === 'Just')
	{
		xhr.timeout = request.timeout._0;
	}
}

function send(xhr, body)
{
	switch (body.ctor)
	{
		case 'EmptyBody':
			xhr.send();
			return;

		case 'StringBody':
			xhr.setRequestHeader('Content-Type', body._0);
			xhr.send(body._1);
			return;

		case 'FormDataBody':
			xhr.send(body._0);
			return;
	}
}


// RESPONSES

function handleResponse(xhr, responseToResult)
{
	var response = toResponse(xhr);

	if (xhr.status < 200 || 300 <= xhr.status)
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadStatus',
			_0: response
		});
	}

	var result = responseToResult(response);

	if (result.ctor === 'Ok')
	{
		return _elm_lang$core$Native_Scheduler.succeed(result._0);
	}
	else
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadPayload',
			_0: result._0,
			_1: response
		});
	}
}

function toResponse(xhr)
{
	return {
		status: { code: xhr.status, message: xhr.statusText },
		headers: parseHeaders(xhr.getAllResponseHeaders()),
		url: xhr.responseURL,
		body: xhr.response
	};
}

function parseHeaders(rawHeaders)
{
	var headers = _elm_lang$core$Dict$empty;

	if (!rawHeaders)
	{
		return headers;
	}

	var headerPairs = rawHeaders.split('\u000d\u000a');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf('\u003a\u0020');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(_elm_lang$core$Dict$update, key, function(oldValue) {
				if (oldValue.ctor === 'Just')
				{
					return _elm_lang$core$Maybe$Just(value + ', ' + oldValue._0);
				}
				return _elm_lang$core$Maybe$Just(value);
			}, headers);
		}
	}

	return headers;
}


// EXPECTORS

function expectStringResponse(responseToResult)
{
	return {
		responseType: 'text',
		responseToResult: responseToResult
	};
}

function mapExpect(func, expect)
{
	return {
		responseType: expect.responseType,
		responseToResult: function(response) {
			var convertedResponse = expect.responseToResult(response);
			return A2(_elm_lang$core$Result$map, func, convertedResponse);
		}
	};
}


// BODY

function multipart(parts)
{
	var formData = new FormData();

	while (parts.ctor !== '[]')
	{
		var part = parts._0;
		formData.append(part._0, part._1);
		parts = parts._1;
	}

	return { ctor: 'FormDataBody', _0: formData };
}

return {
	toTask: F2(toTask),
	expectStringResponse: expectStringResponse,
	mapExpect: F2(mapExpect),
	multipart: multipart,
	encodeUri: encodeUri,
	decodeUri: decodeUri
};

}();

var _elm_lang$http$Http_Internal$map = F2(
	function (func, request) {
		return _elm_lang$core$Native_Utils.update(
			request,
			{
				expect: A2(_elm_lang$http$Native_Http.mapExpect, func, request.expect)
			});
	});
var _elm_lang$http$Http_Internal$RawRequest = F7(
	function (a, b, c, d, e, f, g) {
		return {method: a, headers: b, url: c, body: d, expect: e, timeout: f, withCredentials: g};
	});
var _elm_lang$http$Http_Internal$Request = function (a) {
	return {ctor: 'Request', _0: a};
};
var _elm_lang$http$Http_Internal$Expect = {ctor: 'Expect'};
var _elm_lang$http$Http_Internal$FormDataBody = {ctor: 'FormDataBody'};
var _elm_lang$http$Http_Internal$StringBody = F2(
	function (a, b) {
		return {ctor: 'StringBody', _0: a, _1: b};
	});
var _elm_lang$http$Http_Internal$EmptyBody = {ctor: 'EmptyBody'};
var _elm_lang$http$Http_Internal$Header = F2(
	function (a, b) {
		return {ctor: 'Header', _0: a, _1: b};
	});

var _elm_lang$http$Http$decodeUri = _elm_lang$http$Native_Http.decodeUri;
var _elm_lang$http$Http$encodeUri = _elm_lang$http$Native_Http.encodeUri;
var _elm_lang$http$Http$expectStringResponse = _elm_lang$http$Native_Http.expectStringResponse;
var _elm_lang$http$Http$expectJson = function (decoder) {
	return _elm_lang$http$Http$expectStringResponse(
		function (response) {
			return A2(_elm_lang$core$Json_Decode$decodeString, decoder, response.body);
		});
};
var _elm_lang$http$Http$expectString = _elm_lang$http$Http$expectStringResponse(
	function (response) {
		return _elm_lang$core$Result$Ok(response.body);
	});
var _elm_lang$http$Http$multipartBody = _elm_lang$http$Native_Http.multipart;
var _elm_lang$http$Http$stringBody = _elm_lang$http$Http_Internal$StringBody;
var _elm_lang$http$Http$jsonBody = function (value) {
	return A2(
		_elm_lang$http$Http_Internal$StringBody,
		'application/json',
		A2(_elm_lang$core$Json_Encode$encode, 0, value));
};
var _elm_lang$http$Http$emptyBody = _elm_lang$http$Http_Internal$EmptyBody;
var _elm_lang$http$Http$header = _elm_lang$http$Http_Internal$Header;
var _elm_lang$http$Http$request = _elm_lang$http$Http_Internal$Request;
var _elm_lang$http$Http$post = F3(
	function (url, body, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'POST',
				headers: {ctor: '[]'},
				url: url,
				body: body,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$get = F2(
	function (url, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'GET',
				headers: {ctor: '[]'},
				url: url,
				body: _elm_lang$http$Http$emptyBody,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$getString = function (url) {
	return _elm_lang$http$Http$request(
		{
			method: 'GET',
			headers: {ctor: '[]'},
			url: url,
			body: _elm_lang$http$Http$emptyBody,
			expect: _elm_lang$http$Http$expectString,
			timeout: _elm_lang$core$Maybe$Nothing,
			withCredentials: false
		});
};
var _elm_lang$http$Http$toTask = function (_p0) {
	var _p1 = _p0;
	return A2(_elm_lang$http$Native_Http.toTask, _p1._0, _elm_lang$core$Maybe$Nothing);
};
var _elm_lang$http$Http$send = F2(
	function (resultToMessage, request) {
		return A2(
			_elm_lang$core$Task$attempt,
			resultToMessage,
			_elm_lang$http$Http$toTask(request));
	});
var _elm_lang$http$Http$Response = F4(
	function (a, b, c, d) {
		return {url: a, status: b, headers: c, body: d};
	});
var _elm_lang$http$Http$BadPayload = F2(
	function (a, b) {
		return {ctor: 'BadPayload', _0: a, _1: b};
	});
var _elm_lang$http$Http$BadStatus = function (a) {
	return {ctor: 'BadStatus', _0: a};
};
var _elm_lang$http$Http$NetworkError = {ctor: 'NetworkError'};
var _elm_lang$http$Http$Timeout = {ctor: 'Timeout'};
var _elm_lang$http$Http$BadUrl = function (a) {
	return {ctor: 'BadUrl', _0: a};
};
var _elm_lang$http$Http$StringPart = F2(
	function (a, b) {
		return {ctor: 'StringPart', _0: a, _1: b};
	});
var _elm_lang$http$Http$stringPart = _elm_lang$http$Http$StringPart;

var _elm_lang$navigation$Native_Navigation = function() {


// FAKE NAVIGATION

function go(n)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		if (n !== 0)
		{
			history.go(n);
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function pushState(url)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		history.pushState({}, '', url);
		callback(_elm_lang$core$Native_Scheduler.succeed(getLocation()));
	});
}

function replaceState(url)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		history.replaceState({}, '', url);
		callback(_elm_lang$core$Native_Scheduler.succeed(getLocation()));
	});
}


// REAL NAVIGATION

function reloadPage(skipCache)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		document.location.reload(skipCache);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function setLocation(url)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		try
		{
			window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			document.location.reload(false);
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


// GET LOCATION

function getLocation()
{
	var location = document.location;

	return {
		href: location.href,
		host: location.host,
		hostname: location.hostname,
		protocol: location.protocol,
		origin: location.origin,
		port_: location.port,
		pathname: location.pathname,
		search: location.search,
		hash: location.hash,
		username: location.username,
		password: location.password
	};
}


// DETECT IE11 PROBLEMS

function isInternetExplorer11()
{
	return window.navigator.userAgent.indexOf('Trident') !== -1;
}


return {
	go: go,
	setLocation: setLocation,
	reloadPage: reloadPage,
	pushState: pushState,
	replaceState: replaceState,
	getLocation: getLocation,
	isInternetExplorer11: isInternetExplorer11
};

}();

var _elm_lang$navigation$Navigation$replaceState = _elm_lang$navigation$Native_Navigation.replaceState;
var _elm_lang$navigation$Navigation$pushState = _elm_lang$navigation$Native_Navigation.pushState;
var _elm_lang$navigation$Navigation$go = _elm_lang$navigation$Native_Navigation.go;
var _elm_lang$navigation$Navigation$reloadPage = _elm_lang$navigation$Native_Navigation.reloadPage;
var _elm_lang$navigation$Navigation$setLocation = _elm_lang$navigation$Native_Navigation.setLocation;
var _elm_lang$navigation$Navigation_ops = _elm_lang$navigation$Navigation_ops || {};
_elm_lang$navigation$Navigation_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return task2;
			},
			task1);
	});
var _elm_lang$navigation$Navigation$notify = F3(
	function (router, subs, location) {
		var send = function (_p1) {
			var _p2 = _p1;
			return A2(
				_elm_lang$core$Platform$sendToApp,
				router,
				_p2._0(location));
		};
		return A2(
			_elm_lang$navigation$Navigation_ops['&>'],
			_elm_lang$core$Task$sequence(
				A2(_elm_lang$core$List$map, send, subs)),
			_elm_lang$core$Task$succeed(
				{ctor: '_Tuple0'}));
	});
var _elm_lang$navigation$Navigation$cmdHelp = F3(
	function (router, subs, cmd) {
		var _p3 = cmd;
		switch (_p3.ctor) {
			case 'Jump':
				return _elm_lang$navigation$Navigation$go(_p3._0);
			case 'New':
				return A2(
					_elm_lang$core$Task$andThen,
					A2(_elm_lang$navigation$Navigation$notify, router, subs),
					_elm_lang$navigation$Navigation$pushState(_p3._0));
			case 'Modify':
				return A2(
					_elm_lang$core$Task$andThen,
					A2(_elm_lang$navigation$Navigation$notify, router, subs),
					_elm_lang$navigation$Navigation$replaceState(_p3._0));
			case 'Visit':
				return _elm_lang$navigation$Navigation$setLocation(_p3._0);
			default:
				return _elm_lang$navigation$Navigation$reloadPage(_p3._0);
		}
	});
var _elm_lang$navigation$Navigation$killPopWatcher = function (popWatcher) {
	var _p4 = popWatcher;
	if (_p4.ctor === 'Normal') {
		return _elm_lang$core$Process$kill(_p4._0);
	} else {
		return A2(
			_elm_lang$navigation$Navigation_ops['&>'],
			_elm_lang$core$Process$kill(_p4._0),
			_elm_lang$core$Process$kill(_p4._1));
	}
};
var _elm_lang$navigation$Navigation$onSelfMsg = F3(
	function (router, location, state) {
		return A2(
			_elm_lang$navigation$Navigation_ops['&>'],
			A3(_elm_lang$navigation$Navigation$notify, router, state.subs, location),
			_elm_lang$core$Task$succeed(state));
	});
var _elm_lang$navigation$Navigation$subscription = _elm_lang$core$Native_Platform.leaf('Navigation');
var _elm_lang$navigation$Navigation$command = _elm_lang$core$Native_Platform.leaf('Navigation');
var _elm_lang$navigation$Navigation$Location = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return {href: a, host: b, hostname: c, protocol: d, origin: e, port_: f, pathname: g, search: h, hash: i, username: j, password: k};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$navigation$Navigation$State = F2(
	function (a, b) {
		return {subs: a, popWatcher: b};
	});
var _elm_lang$navigation$Navigation$init = _elm_lang$core$Task$succeed(
	A2(
		_elm_lang$navigation$Navigation$State,
		{ctor: '[]'},
		_elm_lang$core$Maybe$Nothing));
var _elm_lang$navigation$Navigation$Reload = function (a) {
	return {ctor: 'Reload', _0: a};
};
var _elm_lang$navigation$Navigation$reload = _elm_lang$navigation$Navigation$command(
	_elm_lang$navigation$Navigation$Reload(false));
var _elm_lang$navigation$Navigation$reloadAndSkipCache = _elm_lang$navigation$Navigation$command(
	_elm_lang$navigation$Navigation$Reload(true));
var _elm_lang$navigation$Navigation$Visit = function (a) {
	return {ctor: 'Visit', _0: a};
};
var _elm_lang$navigation$Navigation$load = function (url) {
	return _elm_lang$navigation$Navigation$command(
		_elm_lang$navigation$Navigation$Visit(url));
};
var _elm_lang$navigation$Navigation$Modify = function (a) {
	return {ctor: 'Modify', _0: a};
};
var _elm_lang$navigation$Navigation$modifyUrl = function (url) {
	return _elm_lang$navigation$Navigation$command(
		_elm_lang$navigation$Navigation$Modify(url));
};
var _elm_lang$navigation$Navigation$New = function (a) {
	return {ctor: 'New', _0: a};
};
var _elm_lang$navigation$Navigation$newUrl = function (url) {
	return _elm_lang$navigation$Navigation$command(
		_elm_lang$navigation$Navigation$New(url));
};
var _elm_lang$navigation$Navigation$Jump = function (a) {
	return {ctor: 'Jump', _0: a};
};
var _elm_lang$navigation$Navigation$back = function (n) {
	return _elm_lang$navigation$Navigation$command(
		_elm_lang$navigation$Navigation$Jump(0 - n));
};
var _elm_lang$navigation$Navigation$forward = function (n) {
	return _elm_lang$navigation$Navigation$command(
		_elm_lang$navigation$Navigation$Jump(n));
};
var _elm_lang$navigation$Navigation$cmdMap = F2(
	function (_p5, myCmd) {
		var _p6 = myCmd;
		switch (_p6.ctor) {
			case 'Jump':
				return _elm_lang$navigation$Navigation$Jump(_p6._0);
			case 'New':
				return _elm_lang$navigation$Navigation$New(_p6._0);
			case 'Modify':
				return _elm_lang$navigation$Navigation$Modify(_p6._0);
			case 'Visit':
				return _elm_lang$navigation$Navigation$Visit(_p6._0);
			default:
				return _elm_lang$navigation$Navigation$Reload(_p6._0);
		}
	});
var _elm_lang$navigation$Navigation$Monitor = function (a) {
	return {ctor: 'Monitor', _0: a};
};
var _elm_lang$navigation$Navigation$program = F2(
	function (locationToMessage, stuff) {
		var init = stuff.init(
			_elm_lang$navigation$Native_Navigation.getLocation(
				{ctor: '_Tuple0'}));
		var subs = function (model) {
			return _elm_lang$core$Platform_Sub$batch(
				{
					ctor: '::',
					_0: _elm_lang$navigation$Navigation$subscription(
						_elm_lang$navigation$Navigation$Monitor(locationToMessage)),
					_1: {
						ctor: '::',
						_0: stuff.subscriptions(model),
						_1: {ctor: '[]'}
					}
				});
		};
		return _elm_lang$html$Html$program(
			{init: init, view: stuff.view, update: stuff.update, subscriptions: subs});
	});
var _elm_lang$navigation$Navigation$programWithFlags = F2(
	function (locationToMessage, stuff) {
		var init = function (flags) {
			return A2(
				stuff.init,
				flags,
				_elm_lang$navigation$Native_Navigation.getLocation(
					{ctor: '_Tuple0'}));
		};
		var subs = function (model) {
			return _elm_lang$core$Platform_Sub$batch(
				{
					ctor: '::',
					_0: _elm_lang$navigation$Navigation$subscription(
						_elm_lang$navigation$Navigation$Monitor(locationToMessage)),
					_1: {
						ctor: '::',
						_0: stuff.subscriptions(model),
						_1: {ctor: '[]'}
					}
				});
		};
		return _elm_lang$html$Html$programWithFlags(
			{init: init, view: stuff.view, update: stuff.update, subscriptions: subs});
	});
var _elm_lang$navigation$Navigation$subMap = F2(
	function (func, _p7) {
		var _p8 = _p7;
		return _elm_lang$navigation$Navigation$Monitor(
			function (_p9) {
				return func(
					_p8._0(_p9));
			});
	});
var _elm_lang$navigation$Navigation$InternetExplorer = F2(
	function (a, b) {
		return {ctor: 'InternetExplorer', _0: a, _1: b};
	});
var _elm_lang$navigation$Navigation$Normal = function (a) {
	return {ctor: 'Normal', _0: a};
};
var _elm_lang$navigation$Navigation$spawnPopWatcher = function (router) {
	var reportLocation = function (_p10) {
		return A2(
			_elm_lang$core$Platform$sendToSelf,
			router,
			_elm_lang$navigation$Native_Navigation.getLocation(
				{ctor: '_Tuple0'}));
	};
	return _elm_lang$navigation$Native_Navigation.isInternetExplorer11(
		{ctor: '_Tuple0'}) ? A3(
		_elm_lang$core$Task$map2,
		_elm_lang$navigation$Navigation$InternetExplorer,
		_elm_lang$core$Process$spawn(
			A3(_elm_lang$dom$Dom_LowLevel$onWindow, 'popstate', _elm_lang$core$Json_Decode$value, reportLocation)),
		_elm_lang$core$Process$spawn(
			A3(_elm_lang$dom$Dom_LowLevel$onWindow, 'hashchange', _elm_lang$core$Json_Decode$value, reportLocation))) : A2(
		_elm_lang$core$Task$map,
		_elm_lang$navigation$Navigation$Normal,
		_elm_lang$core$Process$spawn(
			A3(_elm_lang$dom$Dom_LowLevel$onWindow, 'popstate', _elm_lang$core$Json_Decode$value, reportLocation)));
};
var _elm_lang$navigation$Navigation$onEffects = F4(
	function (router, cmds, subs, _p11) {
		var _p12 = _p11;
		var _p15 = _p12.popWatcher;
		var stepState = function () {
			var _p13 = {ctor: '_Tuple2', _0: subs, _1: _p15};
			_v6_2:
			do {
				if (_p13._0.ctor === '[]') {
					if (_p13._1.ctor === 'Just') {
						return A2(
							_elm_lang$navigation$Navigation_ops['&>'],
							_elm_lang$navigation$Navigation$killPopWatcher(_p13._1._0),
							_elm_lang$core$Task$succeed(
								A2(_elm_lang$navigation$Navigation$State, subs, _elm_lang$core$Maybe$Nothing)));
					} else {
						break _v6_2;
					}
				} else {
					if (_p13._1.ctor === 'Nothing') {
						return A2(
							_elm_lang$core$Task$map,
							function (_p14) {
								return A2(
									_elm_lang$navigation$Navigation$State,
									subs,
									_elm_lang$core$Maybe$Just(_p14));
							},
							_elm_lang$navigation$Navigation$spawnPopWatcher(router));
					} else {
						break _v6_2;
					}
				}
			} while(false);
			return _elm_lang$core$Task$succeed(
				A2(_elm_lang$navigation$Navigation$State, subs, _p15));
		}();
		return A2(
			_elm_lang$navigation$Navigation_ops['&>'],
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					A2(_elm_lang$navigation$Navigation$cmdHelp, router, subs),
					cmds)),
			stepState);
	});
_elm_lang$core$Native_Platform.effectManagers['Navigation'] = {pkg: 'elm-lang/navigation', init: _elm_lang$navigation$Navigation$init, onEffects: _elm_lang$navigation$Navigation$onEffects, onSelfMsg: _elm_lang$navigation$Navigation$onSelfMsg, tag: 'fx', cmdMap: _elm_lang$navigation$Navigation$cmdMap, subMap: _elm_lang$navigation$Navigation$subMap};

var _truqu$elm_base64$BitList$partition = F2(
	function (size, list) {
		if (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(list),
			size) < 1) {
			return {
				ctor: '::',
				_0: list,
				_1: {ctor: '[]'}
			};
		} else {
			var partitionTail = F3(
				function (size, list, res) {
					partitionTail:
					while (true) {
						var _p0 = list;
						if (_p0.ctor === '[]') {
							return res;
						} else {
							var _v1 = size,
								_v2 = A2(_elm_lang$core$List$drop, size, list),
								_v3 = {
								ctor: '::',
								_0: A2(_elm_lang$core$List$take, size, list),
								_1: res
							};
							size = _v1;
							list = _v2;
							res = _v3;
							continue partitionTail;
						}
					}
				});
			return _elm_lang$core$List$reverse(
				A3(
					partitionTail,
					size,
					list,
					{ctor: '[]'}));
		}
	});
var _truqu$elm_base64$BitList$toByteReverse = function (bitList) {
	var _p1 = bitList;
	if (_p1.ctor === '[]') {
		return 0;
	} else {
		if (_p1._0.ctor === 'Off') {
			return 2 * _truqu$elm_base64$BitList$toByteReverse(_p1._1);
		} else {
			return 1 + (2 * _truqu$elm_base64$BitList$toByteReverse(_p1._1));
		}
	}
};
var _truqu$elm_base64$BitList$toByte = function (bitList) {
	return _truqu$elm_base64$BitList$toByteReverse(
		_elm_lang$core$List$reverse(bitList));
};
var _truqu$elm_base64$BitList$Off = {ctor: 'Off'};
var _truqu$elm_base64$BitList$On = {ctor: 'On'};
var _truqu$elm_base64$BitList$fromNumber = function ($int) {
	return _elm_lang$core$Native_Utils.eq($int, 0) ? {ctor: '[]'} : (_elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], $int, 2),
		1) ? A2(
		_elm_lang$core$List$append,
		_truqu$elm_base64$BitList$fromNumber(($int / 2) | 0),
		{
			ctor: '::',
			_0: _truqu$elm_base64$BitList$On,
			_1: {ctor: '[]'}
		}) : A2(
		_elm_lang$core$List$append,
		_truqu$elm_base64$BitList$fromNumber(($int / 2) | 0),
		{
			ctor: '::',
			_0: _truqu$elm_base64$BitList$Off,
			_1: {ctor: '[]'}
		}));
};
var _truqu$elm_base64$BitList$fromNumberWithSize = F2(
	function (number, size) {
		var bitList = _truqu$elm_base64$BitList$fromNumber(number);
		var paddingSize = size - _elm_lang$core$List$length(bitList);
		return A2(
			_elm_lang$core$List$append,
			A2(_elm_lang$core$List$repeat, paddingSize, _truqu$elm_base64$BitList$Off),
			bitList);
	});
var _truqu$elm_base64$BitList$fromByte = function ($byte) {
	return A2(_truqu$elm_base64$BitList$fromNumberWithSize, $byte, 8);
};

var _truqu$elm_base64$Base64$dropLast = F2(
	function (number, list) {
		return _elm_lang$core$List$reverse(
			A2(
				_elm_lang$core$List$drop,
				number,
				_elm_lang$core$List$reverse(list)));
	});
var _truqu$elm_base64$Base64$partitionBits = function (list) {
	var list_ = A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		A2(_elm_lang$core$List$map, _truqu$elm_base64$BitList$fromByte, list));
	return A2(
		_elm_lang$core$List$map,
		_truqu$elm_base64$BitList$toByte,
		A2(_truqu$elm_base64$BitList$partition, 6, list_));
};
var _truqu$elm_base64$Base64$base64CharsList = _elm_lang$core$String$toList('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/');
var _truqu$elm_base64$Base64$base64Map = function () {
	var insert = F2(
		function (_p0, dict) {
			var _p1 = _p0;
			return A3(_elm_lang$core$Dict$insert, _p1._1, _p1._0, dict);
		});
	return A3(
		_elm_lang$core$List$foldl,
		insert,
		_elm_lang$core$Dict$empty,
		A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			_truqu$elm_base64$Base64$base64CharsList));
}();
var _truqu$elm_base64$Base64$isValid = function (string) {
	var string_ = A2(_elm_lang$core$String$endsWith, '==', string) ? A2(_elm_lang$core$String$dropRight, 2, string) : (A2(_elm_lang$core$String$endsWith, '=', string) ? A2(_elm_lang$core$String$dropRight, 1, string) : string);
	var isBase64Char = function ($char) {
		return A2(_elm_lang$core$Dict$member, $char, _truqu$elm_base64$Base64$base64Map);
	};
	return A2(_elm_lang$core$String$all, isBase64Char, string_);
};
var _truqu$elm_base64$Base64$toBase64BitList = function (string) {
	var endingEquals = A2(_elm_lang$core$String$endsWith, '==', string) ? 2 : (A2(_elm_lang$core$String$endsWith, '=', string) ? 1 : 0);
	var stripped = _elm_lang$core$String$toList(
		A2(_elm_lang$core$String$dropRight, endingEquals, string));
	var base64ToInt = function ($char) {
		var _p2 = A2(_elm_lang$core$Dict$get, $char, _truqu$elm_base64$Base64$base64Map);
		if (_p2.ctor === 'Just') {
			return _p2._0;
		} else {
			return -1;
		}
	};
	var numberList = A2(_elm_lang$core$List$map, base64ToInt, stripped);
	return A2(
		_truqu$elm_base64$Base64$dropLast,
		endingEquals * 2,
		A2(
			_elm_lang$core$List$concatMap,
			A2(_elm_lang$core$Basics$flip, _truqu$elm_base64$BitList$fromNumberWithSize, 6),
			numberList));
};
var _truqu$elm_base64$Base64$toCharList = function (bitList) {
	var array = _elm_lang$core$Array$fromList(_truqu$elm_base64$Base64$base64CharsList);
	var toBase64Char = function (index) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			_elm_lang$core$Native_Utils.chr('!'),
			A2(_elm_lang$core$Array$get, index, array));
	};
	var toChars = function (_p3) {
		var _p4 = _p3;
		var _p5 = {ctor: '_Tuple3', _0: _p4._0, _1: _p4._1, _2: _p4._2};
		if (_p5._2 === -1) {
			if (_p5._1 === -1) {
				return A2(
					_elm_lang$core$List$append,
					A2(
						_truqu$elm_base64$Base64$dropLast,
						2,
						A2(
							_elm_lang$core$List$map,
							toBase64Char,
							_truqu$elm_base64$Base64$partitionBits(
								{
									ctor: '::',
									_0: _p5._0,
									_1: {
										ctor: '::',
										_0: 0,
										_1: {
											ctor: '::',
											_0: 0,
											_1: {ctor: '[]'}
										}
									}
								}))),
					{
						ctor: '::',
						_0: _elm_lang$core$Native_Utils.chr('='),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Native_Utils.chr('='),
							_1: {ctor: '[]'}
						}
					});
			} else {
				return A2(
					_elm_lang$core$List$append,
					A2(
						_truqu$elm_base64$Base64$dropLast,
						1,
						A2(
							_elm_lang$core$List$map,
							toBase64Char,
							_truqu$elm_base64$Base64$partitionBits(
								{
									ctor: '::',
									_0: _p5._0,
									_1: {
										ctor: '::',
										_0: _p5._1,
										_1: {
											ctor: '::',
											_0: 0,
											_1: {ctor: '[]'}
										}
									}
								}))),
					{
						ctor: '::',
						_0: _elm_lang$core$Native_Utils.chr('='),
						_1: {ctor: '[]'}
					});
			}
		} else {
			return A2(
				_elm_lang$core$List$map,
				toBase64Char,
				_truqu$elm_base64$Base64$partitionBits(
					{
						ctor: '::',
						_0: _p5._0,
						_1: {
							ctor: '::',
							_0: _p5._1,
							_1: {
								ctor: '::',
								_0: _p5._2,
								_1: {ctor: '[]'}
							}
						}
					}));
		}
	};
	return A2(_elm_lang$core$List$concatMap, toChars, bitList);
};
var _truqu$elm_base64$Base64$toTupleList = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		if (_p6._1.ctor === '::') {
			if (_p6._1._1.ctor === '::') {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple3', _0: _p6._0, _1: _p6._1._0, _2: _p6._1._1._0},
					_1: _truqu$elm_base64$Base64$toTupleList(_p6._1._1._1)
				};
			} else {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple3', _0: _p6._0, _1: _p6._1._0, _2: -1},
					_1: {ctor: '[]'}
				};
			}
		} else {
			return {
				ctor: '::',
				_0: {ctor: '_Tuple3', _0: _p6._0, _1: -1, _2: -1},
				_1: {ctor: '[]'}
			};
		}
	} else {
		return {ctor: '[]'};
	}
};
var _truqu$elm_base64$Base64$toCodeList = function (string) {
	return A2(
		_elm_lang$core$List$map,
		_elm_lang$core$Char$toCode,
		_elm_lang$core$String$toList(string));
};
var _truqu$elm_base64$Base64$decode = function (s) {
	if (!_truqu$elm_base64$Base64$isValid(s)) {
		return _elm_lang$core$Result$Err('Error while decoding');
	} else {
		var bitList = A2(
			_elm_lang$core$List$map,
			_truqu$elm_base64$BitList$toByte,
			A2(
				_truqu$elm_base64$BitList$partition,
				8,
				_truqu$elm_base64$Base64$toBase64BitList(s)));
		var charList = A2(_elm_lang$core$List$map, _elm_lang$core$Char$fromCode, bitList);
		return _elm_lang$core$Result$Ok(
			_elm_lang$core$String$fromList(charList));
	}
};
var _truqu$elm_base64$Base64$encode = function (s) {
	return _elm_lang$core$Result$Ok(
		_elm_lang$core$String$fromList(
			_truqu$elm_base64$Base64$toCharList(
				_truqu$elm_base64$Base64$toTupleList(
					_truqu$elm_base64$Base64$toCodeList(s)))));
};

var _user$project$AccessTokenParser$firstOccurrence = F2(
	function (c, s) {
		var _p0 = A2(
			_elm_lang$core$String$indexes,
			_elm_lang$core$String$fromChar(c),
			s);
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			return _elm_lang$core$Maybe$Just(_p0._0);
		}
	});
var _user$project$AccessTokenParser$splitAtFirst = F2(
	function (c, s) {
		var _p1 = A2(_user$project$AccessTokenParser$firstOccurrence, c, s);
		if (_p1.ctor === 'Nothing') {
			return {ctor: '_Tuple2', _0: s, _1: ''};
		} else {
			var _p2 = _p1._0;
			return {
				ctor: '_Tuple2',
				_0: A2(_elm_lang$core$String$left, _p2, s),
				_1: A2(_elm_lang$core$String$dropLeft, _p2 + 1, s)
			};
		}
	});
var _user$project$AccessTokenParser$extractAccessToken = function (hash) {
	var eachParam = A2(_elm_lang$core$String$split, '&', hash);
	var eachPair = A2(
		_elm_lang$core$List$map,
		_user$project$AccessTokenParser$splitAtFirst(
			_elm_lang$core$Native_Utils.chr('=')),
		eachParam);
	var allParams = _elm_lang$core$Dict$fromList(eachPair);
	return A2(_elm_lang$core$Dict$get, 'access_token', allParams);
};
var _user$project$AccessTokenParser$parse = function (hash) {
	var _p3 = _elm_lang$core$String$toList(hash);
	if ((_p3.ctor === '::') && (_p3._0.valueOf() === '#')) {
		return _user$project$AccessTokenParser$extractAccessToken(
			_elm_lang$core$String$fromList(_p3._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};

var _user$project$Models$nullOr = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$Models$receiptFormDataToPatch = function (receiptFormData) {
	var tagsUpdate = _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'op',
				_1: _elm_lang$core$Json_Encode$string('replace')
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'path',
					_1: _elm_lang$core$Json_Encode$string('/tags')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'value',
						_1: _elm_lang$core$Json_Encode$list(
							A2(_elm_lang$core$List$map, _elm_lang$core$Json_Encode$string, receiptFormData.tags))
					},
					_1: {ctor: '[]'}
				}
			}
		});
	var transactionTimeUpdate = _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'op',
				_1: _elm_lang$core$Json_Encode$string('replace')
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'path',
					_1: _elm_lang$core$Json_Encode$string('/transactionTime')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'value',
						_1: _elm_lang$core$Json_Encode$int(receiptFormData.transactionTime)
					},
					_1: {ctor: '[]'}
				}
			}
		});
	var descriptionUpdate = _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'op',
				_1: _elm_lang$core$Json_Encode$string('replace')
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'path',
					_1: _elm_lang$core$Json_Encode$string('/description')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'value',
						_1: _elm_lang$core$Json_Encode$string(receiptFormData.description)
					},
					_1: {ctor: '[]'}
				}
			}
		});
	var totalValue = A2(
		_elm_lang$core$Maybe$withDefault,
		_elm_lang$core$Json_Encode$null,
		A2(_elm_lang$core$Maybe$map, _elm_lang$core$Json_Encode$float, receiptFormData.total));
	var totalUpdate = _elm_lang$core$Json_Encode$object(
		{
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'op',
				_1: _elm_lang$core$Json_Encode$string('replace')
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'path',
					_1: _elm_lang$core$Json_Encode$string('/total')
				},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'value', _1: totalValue},
					_1: {ctor: '[]'}
				}
			}
		});
	return _elm_lang$core$Json_Encode$list(
		{
			ctor: '::',
			_0: totalUpdate,
			_1: {
				ctor: '::',
				_0: descriptionUpdate,
				_1: {
					ctor: '::',
					_0: transactionTimeUpdate,
					_1: {
						ctor: '::',
						_0: tagsUpdate,
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _user$project$Models$accessTokenDecoder = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'access_token',
		_1: {ctor: '[]'}
	},
	_elm_lang$core$Json_Decode$string);
var _user$project$Models$UserInfo = F2(
	function (a, b) {
		return {id: a, username: b};
	});
var _user$project$Models$userInfoDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	_user$project$Models$UserInfo,
	A2(_elm_lang$core$Json_Decode$field, 'id', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'userName', _elm_lang$core$Json_Decode$string));
var _user$project$Models$Authentication = F2(
	function (a, b) {
		return {userId: a, token: b};
	});
var _user$project$Models$Receipt = F8(
	function (a, b, c, d, e, f, g, h) {
		return {id: a, userId: b, files: c, timestamp: d, transactionTime: e, total: f, description: g, tags: h};
	});
var _user$project$Models$ReceiptFormData = F4(
	function (a, b, c, d) {
		return {total: a, description: b, transactionTime: c, tags: d};
	});
var _user$project$Models$ReceiptFile = F4(
	function (a, b, c, d) {
		return {id: a, ext: b, metaData: c, timestamp: d};
	});
var _user$project$Models$FileMetadata = F4(
	function (a, b, c, d) {
		return {fileType: a, length: b, width: c, height: d};
	});
var _user$project$Models$fileMetadataDecoder = A5(
	_elm_lang$core$Json_Decode$map4,
	_user$project$Models$FileMetadata,
	A2(_elm_lang$core$Json_Decode$field, 'fileType', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'length', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'width', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'height', _elm_lang$core$Json_Decode$int));
var _user$project$Models$receiptFileDecoder = A5(
	_elm_lang$core$Json_Decode$map4,
	_user$project$Models$ReceiptFile,
	A2(_elm_lang$core$Json_Decode$field, 'id', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'ext', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'metaData', _user$project$Models$fileMetadataDecoder),
	A2(_elm_lang$core$Json_Decode$field, 'timestamp', _elm_lang$core$Json_Decode$int));
var _user$project$Models$receiptDecoder = A9(
	_elm_lang$core$Json_Decode$map8,
	_user$project$Models$Receipt,
	A2(_elm_lang$core$Json_Decode$field, 'id', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'userId', _elm_lang$core$Json_Decode$string),
	A2(
		_elm_lang$core$Json_Decode$field,
		'files',
		_elm_lang$core$Json_Decode$list(_user$project$Models$receiptFileDecoder)),
	A2(_elm_lang$core$Json_Decode$field, 'timestamp', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'transactionTime', _elm_lang$core$Json_Decode$int),
	A2(
		_elm_lang$core$Json_Decode$field,
		'total',
		_user$project$Models$nullOr(_elm_lang$core$Json_Decode$float)),
	A2(_elm_lang$core$Json_Decode$field, 'description', _elm_lang$core$Json_Decode$string),
	A2(
		_elm_lang$core$Json_Decode$field,
		'tags',
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string)));
var _user$project$Models$receiptsDecoder = _elm_lang$core$Json_Decode$list(_user$project$Models$receiptDecoder);
var _user$project$Models$AppConfig = function (a) {
	return {googleClientId: a};
};
var _user$project$Models$appConfigDecoder = A2(
	_elm_lang$core$Json_Decode$map,
	_user$project$Models$AppConfig,
	A2(_elm_lang$core$Json_Decode$field, 'googleClientId', _elm_lang$core$Json_Decode$string));

var _user$project$Ports$receiptFileMouseDown = _elm_lang$core$Native_Platform.outgoingPort(
	'receiptFileMouseDown',
	function (v) {
		return v;
	});
var _user$project$Ports$receiptFileSelected = _elm_lang$core$Native_Platform.incomingPort(
	'receiptFileSelected',
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (isImage) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (imageDataUrl) {
					return _elm_lang$core$Json_Decode$succeed(
						{isImage: isImage, imageDataUrl: imageDataUrl});
				},
				A2(
					_elm_lang$core$Json_Decode$field,
					'imageDataUrl',
					_elm_lang$core$Json_Decode$oneOf(
						{
							ctor: '::',
							_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
							_1: {
								ctor: '::',
								_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _elm_lang$core$Json_Decode$string),
								_1: {ctor: '[]'}
							}
						})));
		},
		A2(_elm_lang$core$Json_Decode$field, 'isImage', _elm_lang$core$Json_Decode$bool)));
var _user$project$Ports$createReceipt = _elm_lang$core$Native_Platform.outgoingPort(
	'createReceipt',
	function (v) {
		return {
			receiptDetails: {
				total: (v.receiptDetails.total.ctor === 'Nothing') ? null : v.receiptDetails.total._0,
				description: v.receiptDetails.description,
				transactionTime: v.receiptDetails.transactionTime,
				tags: _elm_lang$core$Native_List.toArray(v.receiptDetails.tags).map(
					function (v) {
						return v;
					})
			},
			fileInputId: v.fileInputId,
			url: v.url,
			authToken: v.authToken
		};
	});
var _user$project$Ports$receiptCreated = _elm_lang$core$Native_Platform.incomingPort(
	'receiptCreated',
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (receiptId) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (error) {
					return _elm_lang$core$Json_Decode$succeed(
						{receiptId: receiptId, error: error});
				},
				A2(
					_elm_lang$core$Json_Decode$field,
					'error',
					_elm_lang$core$Json_Decode$oneOf(
						{
							ctor: '::',
							_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
							_1: {
								ctor: '::',
								_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _elm_lang$core$Json_Decode$string),
								_1: {ctor: '[]'}
							}
						})));
		},
		A2(_elm_lang$core$Json_Decode$field, 'receiptId', _elm_lang$core$Json_Decode$string)));
var _user$project$Ports$initDownload = _elm_lang$core$Native_Platform.outgoingPort(
	'initDownload',
	function (v) {
		return v;
	});
var _user$project$Ports$showDialog = _elm_lang$core$Native_Platform.outgoingPort(
	'showDialog',
	function (v) {
		return v;
	});
var _user$project$Ports$CreateReceiptParams = F4(
	function (a, b, c, d) {
		return {receiptDetails: a, fileInputId: b, url: c, authToken: d};
	});
var _user$project$Ports$CreateReceiptResult = F2(
	function (a, b) {
		return {receiptId: a, error: b};
	});
var _user$project$Ports$FileToUpload = F2(
	function (a, b) {
		return {isImage: a, imageDataUrl: b};
	});

var _user$project$Api$basicAuthHeader = F2(
	function (username, password) {
		return A2(
			_elm_lang$core$Result$map,
			function (s) {
				return A2(_elm_lang$core$Basics_ops['++'], 'Basic ', s);
			},
			_truqu$elm_base64$Base64$encode(
				A2(
					_elm_lang$core$Basics_ops['++'],
					username,
					A2(_elm_lang$core$Basics_ops['++'], ':', password))));
	});
var _user$project$Api$authorizationHeaders = function (token) {
	return A2(
		_elm_lang$http$Http$header,
		'Authorization',
		A2(_elm_lang$core$Basics_ops['++'], 'Bearer ', token));
};
var _user$project$Api$baseUrl = 'http://localhost:9000';
var _user$project$Api$createReceiptUrl = function (userId) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_user$project$Api$baseUrl,
		A2(
			_elm_lang$core$Basics_ops['++'],
			'/user/',
			A2(_elm_lang$core$Basics_ops['++'], userId, '/receipt')));
};
var _user$project$Api$receiptFileUrl = F4(
	function (userId, receiptId, fileId, ext) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			_user$project$Api$baseUrl,
			A2(
				_elm_lang$core$Basics_ops['++'],
				'/user/',
				A2(
					_elm_lang$core$Basics_ops['++'],
					userId,
					A2(
						_elm_lang$core$Basics_ops['++'],
						'/receipt/',
						A2(
							_elm_lang$core$Basics_ops['++'],
							receiptId,
							A2(
								_elm_lang$core$Basics_ops['++'],
								'/file/',
								A2(
									_elm_lang$core$Basics_ops['++'],
									fileId,
									A2(_elm_lang$core$Basics_ops['++'], '.', ext))))))));
	});
var _user$project$Api$Error = function (a) {
	return {ctor: 'Error', _0: a};
};
var _user$project$Api$transformHttpError = function (httpError) {
	var _p0 = httpError;
	switch (_p0.ctor) {
		case 'Timeout':
			return _user$project$Api$Error('Timeout');
		case 'NetworkError':
			return _user$project$Api$Error('NetworkError');
		case 'BadPayload':
			return _user$project$Api$Error(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'BadPayload ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_p0._0,
						A2(_elm_lang$core$Basics_ops['++'], ' ', _p0._1.body))));
		case 'BadStatus':
			return _user$project$Api$Error(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'BadStatus ',
					A2(_elm_lang$core$Basics_ops['++'], ' ', _p0._0.body)));
		default:
			return _user$project$Api$Error(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'BadUrl ',
					A2(_elm_lang$core$Basics_ops['++'], ' ', _p0._0)));
	}
};
var _user$project$Api$transformResultHandler = F2(
	function (toMsg, result) {
		return toMsg(
			A2(_elm_lang$core$Result$mapError, _user$project$Api$transformHttpError, result));
	});
var _user$project$Api$fetchAppConfig = function (handler) {
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Api$transformResultHandler(handler),
		A2(
			_elm_lang$http$Http$get,
			A2(_elm_lang$core$Basics_ops['++'], _user$project$Api$baseUrl, '/config'),
			_user$project$Models$appConfigDecoder));
};
var _user$project$Api$fetchBackupUrl = F2(
	function (authentication, handler) {
		var request = _elm_lang$http$Http$request(
			{
				method: 'GET',
				headers: {
					ctor: '::',
					_0: _user$project$Api$authorizationHeaders(authentication.token),
					_1: {ctor: '[]'}
				},
				url: A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Api$baseUrl,
					A2(
						_elm_lang$core$Basics_ops['++'],
						'/user/',
						A2(_elm_lang$core$Basics_ops['++'], authentication.userId, '/backup/token'))),
				body: _elm_lang$http$Http$emptyBody,
				expect: _elm_lang$http$Http$expectJson(_user$project$Models$accessTokenDecoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: true
			});
		var accessTokenTask = _elm_lang$http$Http$toTask(request);
		var backupUrlTask = A2(
			_elm_lang$core$Task$map,
			function (token) {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Api$baseUrl,
					A2(
						_elm_lang$core$Basics_ops['++'],
						'/user/',
						A2(
							_elm_lang$core$Basics_ops['++'],
							authentication.userId,
							A2(_elm_lang$core$Basics_ops['++'], '/backup/download?access_token=', token))));
			},
			accessTokenTask);
		return A2(
			_elm_lang$core$Task$attempt,
			_user$project$Api$transformResultHandler(handler),
			backupUrlTask);
	});
var _user$project$Api$authenticationGet = F2(
	function (basicAuthHeader, handler) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Api$transformResultHandler(handler),
			_elm_lang$http$Http$request(
				{
					method: 'GET',
					headers: {
						ctor: '::',
						_0: A2(_elm_lang$http$Http$header, 'Authorization', basicAuthHeader),
						_1: {ctor: '[]'}
					},
					url: A2(_elm_lang$core$Basics_ops['++'], _user$project$Api$baseUrl, '/token/create'),
					body: _elm_lang$http$Http$emptyBody,
					expect: _elm_lang$http$Http$expectJson(_user$project$Models$accessTokenDecoder),
					timeout: _elm_lang$core$Maybe$Nothing,
					withCredentials: true
				}));
	});
var _user$project$Api$authenticate = F3(
	function (username, password, handler) {
		var basicAuthHeaderResult = A2(_user$project$Api$basicAuthHeader, username, password);
		var _p1 = basicAuthHeaderResult;
		if (_p1.ctor === 'Ok') {
			return A2(_user$project$Api$authenticationGet, _p1._0, handler);
		} else {
			return _elm_lang$core$Platform_Cmd$none;
		}
	});
var _user$project$Api$authenticateWithGoogle = F2(
	function (accessToken, handler) {
		var accessTokenValue = _elm_lang$core$Json_Encode$object(
			{
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'token',
					_1: _elm_lang$core$Json_Encode$string(accessToken)
				},
				_1: {ctor: '[]'}
			});
		var request = _elm_lang$http$Http$request(
			{
				method: 'POST',
				headers: {
					ctor: '::',
					_0: A2(_elm_lang$http$Http$header, 'Accept', 'application/json'),
					_1: {ctor: '[]'}
				},
				url: A2(_elm_lang$core$Basics_ops['++'], _user$project$Api$baseUrl, '/oauth/google-access-token'),
				body: _elm_lang$http$Http$jsonBody(accessTokenValue),
				expect: _elm_lang$http$Http$expectJson(_user$project$Models$accessTokenDecoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: true
			});
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Api$transformResultHandler(handler),
			request);
	});
var _user$project$Api$fetchUserInfo = F2(
	function (token, handler) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Api$transformResultHandler(handler),
			_elm_lang$http$Http$request(
				{
					method: 'GET',
					headers: {
						ctor: '::',
						_0: _user$project$Api$authorizationHeaders(token),
						_1: {ctor: '[]'}
					},
					url: A2(_elm_lang$core$Basics_ops['++'], _user$project$Api$baseUrl, '/user/info'),
					body: _elm_lang$http$Http$emptyBody,
					expect: _elm_lang$http$Http$expectJson(_user$project$Models$userInfoDecoder),
					timeout: _elm_lang$core$Maybe$Nothing,
					withCredentials: true
				}));
	});
var _user$project$Api$fetchReceipts = F2(
	function (authentication, handler) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Api$transformResultHandler(handler),
			_elm_lang$http$Http$request(
				{
					method: 'GET',
					headers: {
						ctor: '::',
						_0: _user$project$Api$authorizationHeaders(authentication.token),
						_1: {ctor: '[]'}
					},
					url: A2(
						_elm_lang$core$Basics_ops['++'],
						_user$project$Api$baseUrl,
						A2(
							_elm_lang$core$Basics_ops['++'],
							'/user/',
							A2(_elm_lang$core$Basics_ops['++'], authentication.userId, '/receipt'))),
					body: _elm_lang$http$Http$emptyBody,
					expect: _elm_lang$http$Http$expectJson(_user$project$Models$receiptsDecoder),
					timeout: _elm_lang$core$Maybe$Nothing,
					withCredentials: true
				}));
	});
var _user$project$Api$updateReceipt = F4(
	function (authentication, receiptId, receiptFormData, handler) {
		var request = _elm_lang$http$Http$request(
			{
				method: 'PATCH',
				headers: {
					ctor: '::',
					_0: _user$project$Api$authorizationHeaders(authentication.token),
					_1: {ctor: '[]'}
				},
				url: A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Api$baseUrl,
					A2(
						_elm_lang$core$Basics_ops['++'],
						'/user/',
						A2(
							_elm_lang$core$Basics_ops['++'],
							authentication.userId,
							A2(_elm_lang$core$Basics_ops['++'], '/receipt/', receiptId)))),
				body: _elm_lang$http$Http$jsonBody(
					_user$project$Models$receiptFormDataToPatch(receiptFormData)),
				expect: _elm_lang$http$Http$expectJson(_user$project$Models$receiptDecoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: true
			});
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Api$transformResultHandler(handler),
			request);
	});

var _user$project$AddReceiptForm$receiptFormData = function (time) {
	return {
		total: _elm_lang$core$Maybe$Nothing,
		description: '',
		transactionTime: _elm_lang$core$Basics$round(
			_elm_lang$core$Time$inMilliseconds(time)),
		tags: {ctor: '[]'}
	};
};
var _user$project$AddReceiptForm$fileInputId = 'receipt-file';
var _user$project$AddReceiptForm$createReceiptParams = function (model) {
	return {
		receiptDetails: _user$project$AddReceiptForm$receiptFormData(model.currentTime),
		fileInputId: _user$project$AddReceiptForm$fileInputId,
		url: _user$project$Api$createReceiptUrl(model.authentication.userId),
		authToken: model.authentication.token
	};
};
var _user$project$AddReceiptForm$update = F2(
	function (msg, model) {
		var _p0 = msg;
		switch (_p0.ctor) {
			case 'CurrentTime':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							currentTime: _elm_lang$core$Time$inMilliseconds(_p0._0)
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'UploadReceipt':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{uploading: true}),
					_1: _user$project$Ports$createReceipt(
						_user$project$AddReceiptForm$createReceiptParams(model))
				};
			case 'ReceiptFileChange':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{fileSelected: true}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'ReceiptUploaded':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{uploading: false}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			default:
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: _user$project$Ports$receiptFileMouseDown(_user$project$AddReceiptForm$fileInputId)
				};
		}
	});
var _user$project$AddReceiptForm$Model = F4(
	function (a, b, c, d) {
		return {authentication: a, fileSelected: b, currentTime: c, uploading: d};
	});
var _user$project$AddReceiptForm$ReceiptUploaded = function (a) {
	return {ctor: 'ReceiptUploaded', _0: a};
};
var _user$project$AddReceiptForm$ReceiptFileChange = function (a) {
	return {ctor: 'ReceiptFileChange', _0: a};
};
var _user$project$AddReceiptForm$subscriptions = _elm_lang$core$Platform_Sub$batch(
	{
		ctor: '::',
		_0: _user$project$Ports$receiptFileSelected(_user$project$AddReceiptForm$ReceiptFileChange),
		_1: {
			ctor: '::',
			_0: _user$project$Ports$receiptCreated(_user$project$AddReceiptForm$ReceiptUploaded),
			_1: {ctor: '[]'}
		}
	});
var _user$project$AddReceiptForm$ReceiptFileInputStart = {ctor: 'ReceiptFileInputStart'};
var _user$project$AddReceiptForm$UploadReceipt = {ctor: 'UploadReceipt'};
var _user$project$AddReceiptForm$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$input,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$type_('file'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$id(_user$project$AddReceiptForm$fileInputId),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onMouseDown(_user$project$AddReceiptForm$ReceiptFileInputStart),
							_1: {ctor: '[]'}
						}
					}
				},
				{ctor: '[]'}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$button,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onClick(_user$project$AddReceiptForm$UploadReceipt),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$disabled(!model.fileSelected),
							_1: {ctor: '[]'}
						}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('Create receipt'),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$AddReceiptForm$CurrentTime = function (a) {
	return {ctor: 'CurrentTime', _0: a};
};
var _user$project$AddReceiptForm$init = function (authentication) {
	var timeCmd = A2(_elm_lang$core$Task$perform, _user$project$AddReceiptForm$CurrentTime, _elm_lang$core$Time$now);
	var cmds = _elm_lang$core$Platform_Cmd$batch(
		{
			ctor: '::',
			_0: timeCmd,
			_1: {ctor: '[]'}
		});
	var model = {authentication: authentication, fileSelected: false, currentTime: 0, uploading: false};
	return {ctor: '_Tuple2', _0: model, _1: cmds};
};

var _user$project$MousePosition$target = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'target', decoder);
};
var _user$project$MousePosition$offsetHeight = A2(_elm_lang$core$Json_Decode$field, 'offsetHeight', _elm_lang$core$Json_Decode$float);
var _user$project$MousePosition$offsetWidth = A2(_elm_lang$core$Json_Decode$field, 'offsetWidth', _elm_lang$core$Json_Decode$float);
var _user$project$MousePosition$Target = F2(
	function (a, b) {
		return {offsetWidth: a, offsetHeight: b};
	});
var _user$project$MousePosition$targetDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	_user$project$MousePosition$Target,
	A2(_elm_lang$core$Json_Decode$field, 'offsetWidth', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'offsetHeight', _elm_lang$core$Json_Decode$int));
var _user$project$MousePosition$Offset = F5(
	function (a, b, c, d, e) {
		return {offsetX: a, offsetY: b, pageX: c, pageY: d, target: e};
	});
var _user$project$MousePosition$eventToOffsetDecoder = A6(
	_elm_lang$core$Json_Decode$map5,
	_user$project$MousePosition$Offset,
	A2(_elm_lang$core$Json_Decode$field, 'offsetX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'offsetY', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'pageX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'pageY', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'target', _user$project$MousePosition$targetDecoder));
var _user$project$MousePosition$onMouseMove = function (target) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousemove',
		A2(_elm_lang$core$Json_Decode$map, target, _user$project$MousePosition$eventToOffsetDecoder));
};
var _user$project$MousePosition$onMouseDown = function (target) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		A2(_elm_lang$core$Json_Decode$map, target, _user$project$MousePosition$eventToOffsetDecoder));
};
var _user$project$MousePosition$onMouseUp = function (target) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		A2(_elm_lang$core$Json_Decode$map, target, _user$project$MousePosition$eventToOffsetDecoder));
};

var _user$project$DateTimePicker$timestamp = function (model) {
	return _elm_lang$core$Basics$round(
		_elm_lang$core$Date$toTime(
			_Leonti$elm_time_picker$TimePicker$selectedTime(model.timePickerModel)));
};
var _user$project$DateTimePicker$updateTimePickerModel = F2(
	function (model, date) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{date: date});
	});
var _user$project$DateTimePicker$updateDatePickerModel = F2(
	function (model, date) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{date: date});
	});
var _user$project$DateTimePicker$update = F2(
	function (msg, model) {
		var _p0 = msg;
		if (_p0.ctor === 'TimePickerMsg') {
			var timePickerModel = A2(_Leonti$elm_time_picker$TimePicker$update, _p0._0, model.timePickerModel);
			return _elm_lang$core$Native_Utils.update(
				model,
				{
					timePickerModel: timePickerModel,
					datePickerModel: A2(
						_user$project$DateTimePicker$updateDatePickerModel,
						model.datePickerModel,
						_Leonti$elm_time_picker$TimePicker$selectedTime(timePickerModel))
				});
		} else {
			var datePickerModel = A2(_Leonti$elm_material_datepicker$DatePicker$update, _p0._0, model.datePickerModel);
			return _elm_lang$core$Native_Utils.update(
				model,
				{
					datePickerModel: datePickerModel,
					timePickerModel: A2(
						_user$project$DateTimePicker$updateTimePickerModel,
						model.timePickerModel,
						_Leonti$elm_material_datepicker$DatePicker$selectedDate(datePickerModel))
				});
		}
	});
var _user$project$DateTimePicker$init = function (dateTime) {
	var datePickerModel = A2(_Leonti$elm_material_datepicker$DatePicker$init, dateTime, '#00bcd4');
	var timePickerModel = A2(
		_Leonti$elm_time_picker$TimePicker$init,
		{is24Hours: true, mainColor: '#00bcd4'},
		dateTime);
	return {timePickerModel: timePickerModel, datePickerModel: datePickerModel};
};
var _user$project$DateTimePicker$Model = F2(
	function (a, b) {
		return {datePickerModel: a, timePickerModel: b};
	});
var _user$project$DateTimePicker$DatePickerMsg = function (a) {
	return {ctor: 'DatePickerMsg', _0: a};
};
var _user$project$DateTimePicker$TimePickerMsg = function (a) {
	return {ctor: 'TimePickerMsg', _0: a};
};
var _user$project$DateTimePicker$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('date-time-picker'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$map,
				_user$project$DateTimePicker$DatePickerMsg,
				_Leonti$elm_material_datepicker$DatePicker$view(model.datePickerModel)),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$map,
					_user$project$DateTimePicker$TimePickerMsg,
					_Leonti$elm_time_picker$TimePicker$view(model.timePickerModel)),
				_1: {ctor: '[]'}
			}
		});
};

var _user$project$FormatUtils$formatMoney = function (amount) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'$',
		_elm_lang$core$Basics$toString(amount));
};
var _user$project$FormatUtils$formatTimestamp = function (timeStamp) {
	return A3(
		_rluiten$elm_date_extra$Date_Extra_Format$format,
		_rluiten$elm_date_extra$Date_Extra_Config_Config_en_au$config,
		'%Y-%m-%d %H:%M',
		_elm_lang$core$Date$fromTime(
			_elm_lang$core$Basics$toFloat(timeStamp)));
};

var _user$project$ReceiptView$zoomWindowView = F3(
	function (maybeImageUrl, receiptFile, zoomBox) {
		var w = 300;
		var imageScale = _elm_lang$core$Basics$toFloat(w) / _elm_lang$core$Basics$toFloat(zoomBox.w);
		var imageWidth = _elm_lang$core$Basics$toFloat(receiptFile.metaData.width) * imageScale;
		var imageHeight = _elm_lang$core$Basics$toFloat(receiptFile.metaData.height) * imageScale;
		var imageTop = 0 - (_elm_lang$core$Basics$toFloat(zoomBox.y) * imageScale);
		var imageLeft = 0 - (_elm_lang$core$Basics$toFloat(zoomBox.x) * imageScale);
		var imageStyle = {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'top',
				_1: A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(imageTop),
					'px')
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'left',
					_1: A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(imageLeft),
						'px')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'width',
						_1: A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(imageWidth),
							'px')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'height',
							_1: A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(imageHeight),
								'px')
						},
						_1: {ctor: '[]'}
					}
				}
			}
		};
		var ratio = _elm_lang$core$Basics$toFloat(zoomBox.w) / _elm_lang$core$Basics$toFloat(zoomBox.h);
		var h = 300 / ratio;
		var zoomWindowStyle = {
			ctor: '::',
			_0: {
				ctor: '_Tuple2',
				_0: 'width',
				_1: A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(w),
					'px')
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'height',
					_1: A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(h),
						'px')
				},
				_1: {ctor: '[]'}
			}
		};
		var imageUrl = A2(_elm_lang$core$Maybe$withDefault, '', maybeImageUrl);
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('zoom-window'),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(zoomWindowStyle),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$img,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('zoomed-image'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$src(imageUrl),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$style(imageStyle),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			});
	});
var _user$project$ReceiptView$dialogTitle = function (editMode) {
	var _p0 = editMode;
	switch (_p0.ctor) {
		case 'EditTotal':
			return 'Total';
		case 'EditTransactionTime':
			return 'Transaction Time';
		default:
			return 'None';
	}
};
var _user$project$ReceiptView$updateSelectionBox = F2(
	function (offset, selectionBox) {
		var y = offset.pageY - selectionBox.deltaY;
		var x = offset.pageX - selectionBox.deltaX;
		return _elm_lang$core$Native_Utils.update(
			selectionBox,
			{w: x - selectionBox.x, h: y - selectionBox.y});
	});
var _user$project$ReceiptView$initialScalingBox = F2(
	function (offset, receiptFile) {
		return {
			deltaX: offset.pageX - offset.offsetX,
			deltaY: offset.pageY - offset.offsetY,
			x: offset.offsetX,
			y: offset.offsetY,
			w: 0,
			h: 0,
			scaling: _elm_lang$core$Basics$toFloat(offset.target.offsetHeight) / _elm_lang$core$Basics$toFloat(receiptFile.metaData.height)
		};
	});
var _user$project$ReceiptView$toZoomBox = function (selectionBox) {
	return {
		x: _elm_lang$core$Basics$round(
			_elm_lang$core$Basics$toFloat(selectionBox.x) / selectionBox.scaling),
		y: _elm_lang$core$Basics$round(
			_elm_lang$core$Basics$toFloat(selectionBox.y) / selectionBox.scaling),
		w: _elm_lang$core$Basics$round(
			_elm_lang$core$Basics$toFloat(selectionBox.w) / selectionBox.scaling),
		h: _elm_lang$core$Basics$round(
			_elm_lang$core$Basics$toFloat(selectionBox.h) / selectionBox.scaling)
	};
};
var _user$project$ReceiptView$delay = F2(
	function (time, msg) {
		return A2(
			_elm_lang$core$Task$perform,
			_elm_lang$core$Basics$identity,
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Basics$always(
					_elm_lang$core$Task$succeed(msg)),
				_elm_lang$core$Process$sleep(time)));
	});
var _user$project$ReceiptView$updatedReceipt = function (msg) {
	var _p1 = msg;
	if ((_p1.ctor === 'UpdateReceiptResult') && (_p1._0.ctor === 'Ok')) {
		return _elm_lang$core$Maybe$Just(_p1._0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _user$project$ReceiptView$isReceiptClosed = function (msg) {
	var _p2 = msg;
	if (_p2.ctor === 'CloseReceipt') {
		return true;
	} else {
		return false;
	}
};
var _user$project$ReceiptView$toFile = function (receipt) {
	var _p3 = receipt.files;
	if (_p3.ctor === '::') {
		if (_p3._1.ctor === '[]') {
			return _elm_lang$core$Maybe$Just(_p3._0);
		} else {
			if (_p3._1._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p3._1._0);
			} else {
				return _elm_lang$core$Maybe$Just(_p3._1._0);
			}
		}
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _user$project$ReceiptView$toImageUrl = F2(
	function (authentication, receipt) {
		return A2(
			_elm_lang$core$Maybe$map,
			function (file) {
				return A4(_user$project$Api$receiptFileUrl, authentication.userId, receipt.id, file.id, file.ext);
			},
			_user$project$ReceiptView$toFile(receipt));
	});
var _user$project$ReceiptView$SelectionBox = F7(
	function (a, b, c, d, e, f, g) {
		return {deltaX: a, deltaY: b, x: c, y: d, w: e, h: f, scaling: g};
	});
var _user$project$ReceiptView$ZoomBox = F4(
	function (a, b, c, d) {
		return {x: a, y: b, w: c, h: d};
	});
var _user$project$ReceiptView$Model = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return {authentication: a, receipt: b, imageUrl: c, selectionBox: d, zoomBox: e, receiptFormData: f, total: g, editMode: h, updatingReceipt: i, dateTimePickerModel: j, mdl: k};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _user$project$ReceiptView$None = {ctor: 'None'};
var _user$project$ReceiptView$EditTransactionTime = {ctor: 'EditTransactionTime'};
var _user$project$ReceiptView$EditTotal = {ctor: 'EditTotal'};
var _user$project$ReceiptView$switchMode = function (editMode) {
	var _p4 = editMode;
	switch (_p4.ctor) {
		case 'EditTotal':
			return _user$project$ReceiptView$EditTransactionTime;
		case 'EditTransactionTime':
			return _user$project$ReceiptView$EditTotal;
		default:
			return _user$project$ReceiptView$None;
	}
};
var _user$project$ReceiptView$toEditMode = function (selectionBox) {
	var ratio = _elm_lang$core$Basics$toFloat(selectionBox.w) / _elm_lang$core$Basics$toFloat(selectionBox.h);
	return (_elm_lang$core$Native_Utils.cmp(ratio, 5.5) > 0) ? _user$project$ReceiptView$EditTransactionTime : _user$project$ReceiptView$EditTotal;
};
var _user$project$ReceiptView$Mdl = function (a) {
	return {ctor: 'Mdl', _0: a};
};
var _user$project$ReceiptView$DateTimePickerMsg = function (a) {
	return {ctor: 'DateTimePickerMsg', _0: a};
};
var _user$project$ReceiptView$UpdateReceiptResult = function (a) {
	return {ctor: 'UpdateReceiptResult', _0: a};
};
var _user$project$ReceiptView$update = F2(
	function (msg, model) {
		var _p5 = msg;
		switch (_p5.ctor) {
			case 'TotalChange':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{total: _p5._0}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'DescriptionChange':
				var receiptFormData = model.receiptFormData;
				var updated = _elm_lang$core$Native_Utils.update(
					receiptFormData,
					{description: _p5._0});
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{receiptFormData: updated}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'MouseDown':
				var selectionBox = A2(
					_elm_lang$core$Maybe$map,
					_user$project$ReceiptView$initialScalingBox(_p5._0),
					_user$project$ReceiptView$toFile(model.receipt));
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{selectionBox: selectionBox}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'SetImageUrl':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							imageUrl: A2(_user$project$ReceiptView$toImageUrl, model.authentication, model.receipt)
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'EditModeSwitch':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							editMode: _user$project$ReceiptView$switchMode(model.editMode)
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'MouseMove':
				var selectionBox = A2(
					_elm_lang$core$Maybe$map,
					_user$project$ReceiptView$updateSelectionBox(_p5._0),
					model.selectionBox);
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{selectionBox: selectionBox}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'MouseUp':
				var editMode = A2(
					_elm_lang$core$Maybe$withDefault,
					_user$project$ReceiptView$None,
					A2(_elm_lang$core$Maybe$map, _user$project$ReceiptView$toEditMode, model.selectionBox));
				var cmd = _elm_lang$core$Native_Utils.eq(model.editMode, _user$project$ReceiptView$None) ? _elm_lang$core$Platform_Cmd$none : _user$project$Ports$showDialog(
					_elm_lang$core$Basics$toString(model.editMode));
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							selectionBox: _elm_lang$core$Maybe$Nothing,
							zoomBox: A2(_elm_lang$core$Maybe$map, _user$project$ReceiptView$toZoomBox, model.selectionBox),
							editMode: editMode
						}),
					_1: cmd
				};
			case 'Mdl':
				return A3(_debois$elm_mdl$Material$update, _user$project$ReceiptView$Mdl, _p5._0, model);
			case 'UpdateReceipt':
				var receiptFormData = model.receiptFormData;
				var updated = _elm_lang$core$Native_Utils.update(
					receiptFormData,
					{
						total: _elm_lang$core$Result$toMaybe(
							_elm_lang$core$String$toFloat(model.total))
					});
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{updatingReceipt: true}),
					_1: A4(_user$project$Api$updateReceipt, model.authentication, model.receipt.id, updated, _user$project$ReceiptView$UpdateReceiptResult)
				};
			case 'UpdateReceiptResult':
				if (_p5._0.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{updatingReceipt: false}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
			case 'CloseReceipt':
				return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
			default:
				var receiptFormData = model.receiptFormData;
				var dateTimePickerModel = A2(_user$project$DateTimePicker$update, _p5._0, model.dateTimePickerModel);
				var updated = _elm_lang$core$Native_Utils.update(
					receiptFormData,
					{
						transactionTime: _user$project$DateTimePicker$timestamp(dateTimePickerModel)
					});
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{dateTimePickerModel: dateTimePickerModel, receiptFormData: updated}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
		}
	});
var _user$project$ReceiptView$CloseReceipt = {ctor: 'CloseReceipt'};
var _user$project$ReceiptView$UpdateReceipt = {ctor: 'UpdateReceipt'};
var _user$project$ReceiptView$EditModeSwitch = {ctor: 'EditModeSwitch'};
var _user$project$ReceiptView$SetImageUrl = {ctor: 'SetImageUrl'};
var _user$project$ReceiptView$init = F2(
	function (authentication, receipt) {
		var receiptFormData = {total: receipt.total, description: receipt.description, transactionTime: receipt.transactionTime, tags: receipt.tags};
		var dateTimePickerModel = _user$project$DateTimePicker$init(
			_elm_lang$core$Date$fromTime(
				_elm_lang$core$Basics$toFloat(receiptFormData.transactionTime)));
		var model = {
			authentication: authentication,
			receipt: receipt,
			imageUrl: _elm_lang$core$Maybe$Nothing,
			selectionBox: _elm_lang$core$Maybe$Nothing,
			zoomBox: _elm_lang$core$Maybe$Nothing,
			receiptFormData: receiptFormData,
			total: A2(
				_elm_lang$core$Maybe$withDefault,
				'',
				A2(_elm_lang$core$Maybe$map, _elm_lang$core$Basics$toString, receipt.total)),
			editMode: _user$project$ReceiptView$None,
			updatingReceipt: false,
			dateTimePickerModel: dateTimePickerModel,
			mdl: _debois$elm_mdl$Material$model
		};
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: A2(_user$project$ReceiptView$delay, 10, _user$project$ReceiptView$SetImageUrl)
		};
	});
var _user$project$ReceiptView$MouseUp = function (a) {
	return {ctor: 'MouseUp', _0: a};
};
var _user$project$ReceiptView$MouseMove = function (a) {
	return {ctor: 'MouseMove', _0: a};
};
var _user$project$ReceiptView$MouseDown = function (a) {
	return {ctor: 'MouseDown', _0: a};
};
var _user$project$ReceiptView$receiptImageView = function (model) {
	var selectorStyle = function () {
		var _p6 = model.selectionBox;
		if (_p6.ctor === 'Just') {
			var _p7 = _p6._0;
			return {
				ctor: '::',
				_0: {
					ctor: '_Tuple2',
					_0: 'top',
					_1: A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(_p7.y),
						'px')
				},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'left',
						_1: A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(_p7.x),
							'px')
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'width',
							_1: A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(_p7.w),
								'px')
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'height',
								_1: A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(_p7.h),
									'px')
							},
							_1: {ctor: '[]'}
						}
					}
				}
			};
		} else {
			return {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'display', _1: 'none'},
				_1: {ctor: '[]'}
			};
		}
	}();
	var imageStyle = {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'width', _1: '100%'},
		_1: {ctor: '[]'}
	};
	var imageUrl = A2(_elm_lang$core$Maybe$withDefault, '', model.imageUrl);
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('receipt-image-wrapper'),
			_1: {
				ctor: '::',
				_0: _user$project$MousePosition$onMouseMove(_user$project$ReceiptView$MouseMove),
				_1: {
					ctor: '::',
					_0: _user$project$MousePosition$onMouseDown(_user$project$ReceiptView$MouseDown),
					_1: {
						ctor: '::',
						_0: _user$project$MousePosition$onMouseUp(_user$project$ReceiptView$MouseUp),
						_1: {ctor: '[]'}
					}
				}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('loading-spinner'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Spinner$spinner(
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Spinner$active(true),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Spinner$singleColor(true),
								_1: {ctor: '[]'}
							}
						}),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$img,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$src(imageUrl),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(imageStyle),
							_1: {ctor: '[]'}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('selection-area'),
							_1: {ctor: '[]'}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('region-selector'),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$style(selectorStyle),
									_1: {ctor: '[]'}
								}
							},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _user$project$ReceiptView$DescriptionChange = function (a) {
	return {ctor: 'DescriptionChange', _0: a};
};
var _user$project$ReceiptView$TotalChange = function (a) {
	return {ctor: 'TotalChange', _0: a};
};
var _user$project$ReceiptView$dialogContent = function (model) {
	var maybeZoomedView = A3(
		_elm_lang$core$Maybe$map2,
		_user$project$ReceiptView$zoomWindowView(model.imageUrl),
		_user$project$ReceiptView$toFile(model.receipt),
		model.zoomBox);
	var zoomedView = A2(
		_elm_lang$core$Maybe$withDefault,
		A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{ctor: '[]'}),
		maybeZoomedView);
	var _p8 = model.editMode;
	switch (_p8.ctor) {
		case 'EditTotal':
			return A2(
				_elm_lang$html$Html$div,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: zoomedView,
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class(''),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A5(
									_debois$elm_mdl$Material_Textfield$render,
									_user$project$ReceiptView$Mdl,
									{
										ctor: '::',
										_0: 2,
										_1: {ctor: '[]'}
									},
									model.mdl,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Textfield$label('Total'),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
											_1: {
												ctor: '::',
												_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '20%'),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Textfield$value(model.total),
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Options$onInput(_user$project$ReceiptView$TotalChange),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									},
									{ctor: '[]'}),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				});
		case 'EditTransactionTime':
			return A2(
				_elm_lang$html$Html$div,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: zoomedView,
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$map,
							_user$project$ReceiptView$DateTimePickerMsg,
							_user$project$DateTimePicker$view(model.dateTimePickerModel)),
						_1: {ctor: '[]'}
					}
				});
		default:
			return A2(
				_elm_lang$html$Html$div,
				{ctor: '[]'},
				{ctor: '[]'});
	}
};
var _user$project$ReceiptView$dialogView = function (model) {
	var dialogWidth = _elm_lang$core$Native_Utils.eq(model.editMode, _user$project$ReceiptView$EditTransactionTime) ? '700px' : '500px';
	return A2(
		_debois$elm_mdl$Material_Dialog$view,
		{
			ctor: '::',
			_0: A2(_debois$elm_mdl$Material_Options$css, 'width', dialogWidth),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_Dialog$title,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(
						_user$project$ReceiptView$dialogTitle(model.editMode)),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Dialog$content,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _user$project$ReceiptView$dialogContent(model),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Dialog$actions,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: A5(
								_debois$elm_mdl$Material_Button$render,
								_user$project$ReceiptView$Mdl,
								{
									ctor: '::',
									_0: 0,
									_1: {ctor: '[]'}
								},
								model.mdl,
								{
									ctor: '::',
									_0: _debois$elm_mdl$Material_Dialog$closeOn('click'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('Close'),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A5(
									_debois$elm_mdl$Material_Button$render,
									_user$project$ReceiptView$Mdl,
									{
										ctor: '::',
										_0: 1,
										_1: {ctor: '[]'}
									},
									model.mdl,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Options$onClick(_user$project$ReceiptView$EditModeSwitch),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('Switch mode'),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						}),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$ReceiptView$receiptFormView = function (model) {
	var updateButton = function () {
		var _p9 = model.updatingReceipt;
		if (_p9 === false) {
			return A5(
				_debois$elm_mdl$Material_Button$render,
				_user$project$ReceiptView$Mdl,
				{
					ctor: '::',
					_0: 4,
					_1: {ctor: '[]'}
				},
				model.mdl,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Button$raised,
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Button$colored,
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Button$ripple,
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Options$css, 'float', 'right'),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onClick(_user$project$ReceiptView$UpdateReceipt),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Save receipt'),
					_1: {ctor: '[]'}
				});
		} else {
			return A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('receipt-update-spinner'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Spinner$spinner(
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Spinner$active(true),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Spinner$singleColor(true),
								_1: {ctor: '[]'}
							}
						}),
					_1: {ctor: '[]'}
				});
		}
	}();
	var formattedDate = _user$project$FormatUtils$formatTimestamp(model.receiptFormData.transactionTime);
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('receipt-form'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('price-wrapper'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A5(
						_debois$elm_mdl$Material_Textfield$render,
						_user$project$ReceiptView$Mdl,
						{
							ctor: '::',
							_0: 0,
							_1: {ctor: '[]'}
						},
						model.mdl,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Textfield$label('Total'),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '20%'),
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Textfield$value(model.total),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Options$onInput(_user$project$ReceiptView$TotalChange),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('receipt-close-button'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A5(
									_debois$elm_mdl$Material_Button$render,
									_user$project$ReceiptView$Mdl,
									{
										ctor: '::',
										_0: 5,
										_1: {ctor: '[]'}
									},
									model.mdl,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Button$minifab,
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Button$ripple,
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Options$onClick(_user$project$ReceiptView$CloseReceipt),
												_1: {ctor: '[]'}
											}
										}
									},
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Icon$i('close'),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A5(
							_debois$elm_mdl$Material_Textfield$render,
							_user$project$ReceiptView$Mdl,
							{
								ctor: '::',
								_0: 1,
								_1: {ctor: '[]'}
							},
							model.mdl,
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Textfield$label('Notes'),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
									_1: {
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '100%'),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Textfield$textarea,
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Textfield$rows(3),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options$onInput(_user$project$ReceiptView$DescriptionChange),
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Textfield$value(model.receiptFormData.description),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A3(
						_debois$elm_mdl$Material_Options$styled,
						_elm_lang$html$Html$p,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Typography$subhead,
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Options$css, 'display', 'inline-block'),
								_1: {ctor: '[]'}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(formattedDate),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: updateButton,
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _user$project$ReceiptView$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _user$project$ReceiptView$receiptFormView(model),
			_1: {
				ctor: '::',
				_0: _user$project$ReceiptView$receiptImageView(model),
				_1: {
					ctor: '::',
					_0: _user$project$ReceiptView$dialogView(model),
					_1: {ctor: '[]'}
				}
			}
		});
};

var _user$project$ReceiptList$emptyRow = A2(
	_debois$elm_mdl$Material_List$li,
	{
		ctor: '::',
		_0: _debois$elm_mdl$Material_List$withBody,
		_1: {ctor: '[]'}
	},
	{
		ctor: '::',
		_0: A2(
			_debois$elm_mdl$Material_List$content,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$span,
					{ctor: '[]'},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_List$body,
						{ctor: '[]'},
						{ctor: '[]'}),
					_1: {ctor: '[]'}
				}
			}),
		_1: {ctor: '[]'}
	});
var _user$project$ReceiptList$subscriptions = _elm_lang$core$Platform_Sub$none;
var _user$project$ReceiptList$updateReceipts = F2(
	function (msg, receipts) {
		var _p0 = _user$project$ReceiptView$updatedReceipt(msg);
		if (_p0.ctor === 'Just') {
			var _p1 = _p0._0;
			return A2(
				_elm_lang$core$List$map,
				function (r) {
					return _elm_lang$core$Native_Utils.eq(r.id, _p1.id) ? _p1 : r;
				},
				receipts);
		} else {
			return receipts;
		}
	});
var _user$project$ReceiptList$Model = F4(
	function (a, b, c, d) {
		return {authentication: a, receipts: b, openedReceiptView: c, loadingReceipts: d};
	});
var _user$project$ReceiptList$OpenReceiptView = function (a) {
	return {ctor: 'OpenReceiptView', _0: a};
};
var _user$project$ReceiptList$receiptRow = function (receipt) {
	var totalElement = function () {
		var _p2 = receipt.total;
		if (_p2.ctor === 'Just') {
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('total-present'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(
						_user$project$FormatUtils$formatMoney(_p2._0)),
					_1: {ctor: '[]'}
				});
		} else {
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('add-details'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Add details'),
					_1: {ctor: '[]'}
				});
		}
	}();
	return A2(
		_debois$elm_mdl$Material_List$li,
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_List$withBody,
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$attribute(
					_elm_lang$html$Html_Events$onClick(
						_user$project$ReceiptList$OpenReceiptView(receipt))),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_List$content,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: totalElement,
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_List$body,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(receipt.description),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_List$content2,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_List$info2,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(
									_user$project$FormatUtils$formatTimestamp(receipt.transactionTime)),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$ReceiptList$receiptList = function (model) {
	return model.loadingReceipts ? _debois$elm_mdl$Material_Spinner$spinner(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Spinner$active(true),
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Spinner$singleColor(true),
				_1: {ctor: '[]'}
			}
		}) : A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('receipt-list'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_List$ul,
				{ctor: '[]'},
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$List$map,
						function (receipt) {
							return _user$project$ReceiptList$receiptRow(receipt);
						},
						model.receipts),
					{
						ctor: '::',
						_0: _user$project$ReceiptList$emptyRow,
						_1: {ctor: '[]'}
					})),
			_1: {ctor: '[]'}
		});
};
var _user$project$ReceiptList$ReceiptViewMsg = function (a) {
	return {ctor: 'ReceiptViewMsg', _0: a};
};
var _user$project$ReceiptList$receiptView = function (model) {
	var _p3 = model.openedReceiptView;
	if (_p3.ctor === 'Just') {
		return A2(
			_elm_lang$html$Html$map,
			_user$project$ReceiptList$ReceiptViewMsg,
			_user$project$ReceiptView$view(_p3._0));
	} else {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{ctor: '[]'});
	}
};
var _user$project$ReceiptList$view = function (model) {
	return A2(
		_debois$elm_mdl$Material_Grid$grid,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_Grid$cell,
				{
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 4),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _user$project$ReceiptList$receiptList(model),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Grid$cell,
					{
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 8),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _user$project$ReceiptList$receiptView(model),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$ReceiptList$FetchResult = function (a) {
	return {ctor: 'FetchResult', _0: a};
};
var _user$project$ReceiptList$update = F2(
	function (msg, model) {
		var _p4 = msg;
		switch (_p4.ctor) {
			case 'Fetch':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{loadingReceipts: true}),
					_1: A2(_user$project$Api$fetchReceipts, model.authentication, _user$project$ReceiptList$FetchResult)
				};
			case 'FetchResult':
				if (_p4._0.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{receipts: _p4._0._0, loadingReceipts: false}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{loadingReceipts: false}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				}
			case 'OpenReceiptView':
				var _p5 = A2(_user$project$ReceiptView$init, model.authentication, _p4._0);
				var receiptViewModel = _p5._0;
				var receiptViewCmd = _p5._1;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							openedReceiptView: _elm_lang$core$Maybe$Just(receiptViewModel)
						}),
					_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$ReceiptList$ReceiptViewMsg, receiptViewCmd)
				};
			default:
				var _p8 = _p4._0;
				var _p6 = model.openedReceiptView;
				if (_p6.ctor === 'Just') {
					var _p7 = A2(_user$project$ReceiptView$update, _p8, _p6._0);
					var receiptViewModel = _p7._0;
					var receiptViewCmd = _p7._1;
					var updatedOpenedReceiptView = _user$project$ReceiptView$isReceiptClosed(_p8) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(receiptViewModel);
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								openedReceiptView: updatedOpenedReceiptView,
								receipts: A2(_user$project$ReceiptList$updateReceipts, _p8, model.receipts)
							}),
						_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$ReceiptList$ReceiptViewMsg, receiptViewCmd)
					};
				} else {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
		}
	});
var _user$project$ReceiptList$Fetch = {ctor: 'Fetch'};
var _user$project$ReceiptList$init = function (authentication) {
	return A2(
		_user$project$ReceiptList$update,
		_user$project$ReceiptList$Fetch,
		{
			authentication: authentication,
			receipts: {ctor: '[]'},
			openedReceiptView: _elm_lang$core$Maybe$Nothing,
			loadingReceipts: false
		});
};

var _user$project$Backup$init = function (authentication) {
	return {
		ctor: '_Tuple2',
		_0: {authentication: authentication, mdl: _debois$elm_mdl$Material$model},
		_1: _elm_lang$core$Platform_Cmd$none
	};
};
var _user$project$Backup$Model = F2(
	function (a, b) {
		return {authentication: a, mdl: b};
	});
var _user$project$Backup$Mdl = function (a) {
	return {ctor: 'Mdl', _0: a};
};
var _user$project$Backup$BackupUrlResult = function (a) {
	return {ctor: 'BackupUrlResult', _0: a};
};
var _user$project$Backup$update = F2(
	function (msg, model) {
		var _p0 = msg;
		switch (_p0.ctor) {
			case 'DownloadBackup':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A2(_user$project$Api$fetchBackupUrl, model.authentication, _user$project$Backup$BackupUrlResult)
				};
			case 'BackupUrlResult':
				if (_p0._0.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: model,
						_1: _user$project$Ports$initDownload(_p0._0._0)
					};
				} else {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
			default:
				return A3(_debois$elm_mdl$Material$update, _user$project$Backup$Mdl, _p0._0, model);
		}
	});
var _user$project$Backup$DownloadBackup = {ctor: 'DownloadBackup'};
var _user$project$Backup$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A5(
				_debois$elm_mdl$Material_Button$render,
				_user$project$Backup$Mdl,
				{
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				},
				model.mdl,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Button$raised,
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Button$ripple,
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$onClick(_user$project$Backup$DownloadBackup),
							_1: {ctor: '[]'}
						}
					}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Download Backup'),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};

var _user$project$AuthenticatedUserView$Model = F5(
	function (a, b, c, d, e) {
		return {authentication: a, receiptListModel: b, backupModel: c, maybeAddReceiptFormModel: d, mdl: e};
	});
var _user$project$AuthenticatedUserView$Mdl = function (a) {
	return {ctor: 'Mdl', _0: a};
};
var _user$project$AuthenticatedUserView$HideNewReceiptForm = {ctor: 'HideNewReceiptForm'};
var _user$project$AuthenticatedUserView$ShowNewReceiptForm = {ctor: 'ShowNewReceiptForm'};
var _user$project$AuthenticatedUserView$AddReceiptFormMsg = function (a) {
	return {ctor: 'AddReceiptFormMsg', _0: a};
};
var _user$project$AuthenticatedUserView$addReceiptFormView = F2(
	function (model, maybeAddReceiptFormModel) {
		var _p0 = maybeAddReceiptFormModel;
		if (_p0.ctor === 'Just') {
			return A2(
				_elm_lang$html$Html$div,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$button,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onClick(_user$project$AuthenticatedUserView$HideNewReceiptForm),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('Close new receipt form'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$map,
							_user$project$AuthenticatedUserView$AddReceiptFormMsg,
							_user$project$AddReceiptForm$view(_p0._0)),
						_1: {ctor: '[]'}
					}
				});
		} else {
			return A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('add-receipt-button'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A5(
						_debois$elm_mdl$Material_Button$render,
						_user$project$AuthenticatedUserView$Mdl,
						{
							ctor: '::',
							_0: 0,
							_1: {ctor: '[]'}
						},
						model.mdl,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Button$fab,
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Button$colored,
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onClick(_user$project$AuthenticatedUserView$ShowNewReceiptForm),
									_1: {ctor: '[]'}
								}
							}
						},
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Icon$i('add'),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				});
		}
	});
var _user$project$AuthenticatedUserView$BackupMsg = function (a) {
	return {ctor: 'BackupMsg', _0: a};
};
var _user$project$AuthenticatedUserView$drawerView = function (model) {
	return A2(
		_elm_lang$html$Html$map,
		_user$project$AuthenticatedUserView$BackupMsg,
		_user$project$Backup$view(model.backupModel));
};
var _user$project$AuthenticatedUserView$ReceiptListMsg = function (a) {
	return {ctor: 'ReceiptListMsg', _0: a};
};
var _user$project$AuthenticatedUserView$init = function (authentication) {
	var _p1 = _user$project$Backup$init(authentication);
	var backupModel = _p1._0;
	var backupCmd = _p1._1;
	var _p2 = _user$project$ReceiptList$init(authentication);
	var receiptListModel = _p2._0;
	var receiptListCmd = _p2._1;
	return {
		ctor: '_Tuple2',
		_0: {authentication: authentication, receiptListModel: receiptListModel, backupModel: backupModel, maybeAddReceiptFormModel: _elm_lang$core$Maybe$Nothing, mdl: _debois$elm_mdl$Material$model},
		_1: _elm_lang$core$Platform_Cmd$batch(
			{
				ctor: '::',
				_0: A2(_elm_lang$core$Platform_Cmd$map, _user$project$AuthenticatedUserView$ReceiptListMsg, receiptListCmd),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$Platform_Cmd$map, _user$project$AuthenticatedUserView$BackupMsg, backupCmd),
					_1: {ctor: '[]'}
				}
			})
	};
};
var _user$project$AuthenticatedUserView$update = F2(
	function (msg, model) {
		var _p3 = msg;
		switch (_p3.ctor) {
			case 'ReceiptListMsg':
				var _p4 = A2(_user$project$ReceiptList$update, _p3._0, model.receiptListModel);
				var receiptListModel = _p4._0;
				var receiptListCmd = _p4._1;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{receiptListModel: receiptListModel}),
					_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$AuthenticatedUserView$ReceiptListMsg, receiptListCmd)
				};
			case 'BackupMsg':
				var _p5 = A2(_user$project$Backup$update, _p3._0, model.backupModel);
				var backupModel = _p5._0;
				var backupCmd = _p5._1;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{backupModel: backupModel}),
					_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$AuthenticatedUserView$BackupMsg, backupCmd)
				};
			case 'AddReceiptFormMsg':
				var _p6 = model.maybeAddReceiptFormModel;
				if (_p6.ctor === 'Just') {
					var _p7 = A2(_user$project$AddReceiptForm$update, _p3._0, _p6._0);
					var addReceiptFormModel = _p7._0;
					var addReceiptFormCmd = _p7._1;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								maybeAddReceiptFormModel: _elm_lang$core$Maybe$Just(addReceiptFormModel)
							}),
						_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$AuthenticatedUserView$AddReceiptFormMsg, addReceiptFormCmd)
					};
				} else {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
			case 'ShowNewReceiptForm':
				var _p8 = _user$project$AddReceiptForm$init(model.authentication);
				var addReceiptFormModel = _p8._0;
				var addReceiptFormCmd = _p8._1;
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							maybeAddReceiptFormModel: _elm_lang$core$Maybe$Just(addReceiptFormModel)
						}),
					_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$AuthenticatedUserView$AddReceiptFormMsg, addReceiptFormCmd)
				};
			case 'HideNewReceiptForm':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{maybeAddReceiptFormModel: _elm_lang$core$Maybe$Nothing}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			default:
				return A3(_debois$elm_mdl$Material$update, _user$project$AuthenticatedUserView$Mdl, _p3._0, model);
		}
	});
var _user$project$AuthenticatedUserView$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$map,
				_user$project$AuthenticatedUserView$ReceiptListMsg,
				_user$project$ReceiptList$view(model.receiptListModel)),
			_1: {
				ctor: '::',
				_0: A2(_user$project$AuthenticatedUserView$addReceiptFormView, model, model.maybeAddReceiptFormModel),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$AuthenticatedUserView$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$batch(
		{
			ctor: '::',
			_0: A2(_elm_lang$core$Platform_Sub$map, _user$project$AuthenticatedUserView$ReceiptListMsg, _user$project$ReceiptList$subscriptions),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Platform_Sub$map, _user$project$AuthenticatedUserView$AddReceiptFormMsg, _user$project$AddReceiptForm$subscriptions),
				_1: {ctor: '[]'}
			}
		});
};

var _user$project$LoginForm$googleOauthUrl = F2(
	function (host, googleClientId) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'https://accounts.google.com/o/oauth2/v2/auth?response_type=token&client_id=',
			A2(
				_elm_lang$core$Basics_ops['++'],
				googleClientId,
				A2(
					_elm_lang$core$Basics_ops['++'],
					'&redirect_uri=',
					A2(_elm_lang$core$Basics_ops['++'], host, '&state=some_state&scope=email profile'))));
	});
var _user$project$LoginForm$token = function (model) {
	return model.token;
};
var _user$project$LoginForm$emptyModel = {username: '', password: '', host: '', token: _elm_lang$core$Maybe$Nothing, basicHeader: '', googleClientId: ''};
var _user$project$LoginForm$Model = F6(
	function (a, b, c, d, e, f) {
		return {username: a, password: b, host: c, token: d, basicHeader: e, googleClientId: f};
	});
var _user$project$LoginForm$LoginResult = function (a) {
	return {ctor: 'LoginResult', _0: a};
};
var _user$project$LoginForm$LoginWithGoogle = function (a) {
	return {ctor: 'LoginWithGoogle', _0: a};
};
var _user$project$LoginForm$Login = {ctor: 'Login'};
var _user$project$LoginForm$AppConfigFetchResult = function (a) {
	return {ctor: 'AppConfigFetchResult', _0: a};
};
var _user$project$LoginForm$update = F2(
	function (msg, model) {
		var _p0 = msg;
		switch (_p0.ctor) {
			case 'Name':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{username: _p0._0}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'Password':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{password: _p0._0}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'AppConfigFetch':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: _user$project$Api$fetchAppConfig(_user$project$LoginForm$AppConfigFetchResult)
				};
			case 'AppConfigFetchResult':
				if (_p0._0.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{googleClientId: _p0._0._0.googleClientId}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
			case 'Login':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A3(_user$project$Api$authenticate, model.username, model.password, _user$project$LoginForm$LoginResult)
				};
			case 'LoginWithGoogle':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A2(_user$project$Api$authenticateWithGoogle, _p0._0, _user$project$LoginForm$LoginResult)
				};
			default:
				if (_p0._0.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								token: _elm_lang$core$Maybe$Just(_p0._0._0)
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
		}
	});
var _user$project$LoginForm$AppConfigFetch = {ctor: 'AppConfigFetch'};
var _user$project$LoginForm$init = F3(
	function (maybeToken, host, hash) {
		var initModel = _elm_lang$core$Native_Utils.update(
			_user$project$LoginForm$emptyModel,
			{token: maybeToken, host: host});
		var _p1 = A2(_user$project$LoginForm$update, _user$project$LoginForm$AppConfigFetch, initModel);
		var modelWithClientId = _p1._0;
		var appConfigCmd = _p1._1;
		var _p2 = _user$project$AccessTokenParser$parse(hash);
		if (_p2.ctor === 'Just') {
			var _p3 = A2(
				_user$project$LoginForm$update,
				_user$project$LoginForm$LoginWithGoogle(_p2._0),
				modelWithClientId);
			var loginModel = _p3._0;
			var loginCmd = _p3._1;
			return {
				ctor: '_Tuple2',
				_0: loginModel,
				_1: _elm_lang$core$Platform_Cmd$batch(
					{
						ctor: '::',
						_0: appConfigCmd,
						_1: {
							ctor: '::',
							_0: loginCmd,
							_1: {ctor: '[]'}
						}
					})
			};
		} else {
			return {ctor: '_Tuple2', _0: modelWithClientId, _1: appConfigCmd};
		}
	});
var _user$project$LoginForm$Password = function (a) {
	return {ctor: 'Password', _0: a};
};
var _user$project$LoginForm$Name = function (a) {
	return {ctor: 'Name', _0: a};
};
var _user$project$LoginForm$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$input,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$type_('text'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$placeholder('Name'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$value(model.username),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Events$onInput(_user$project$LoginForm$Name),
								_1: {ctor: '[]'}
							}
						}
					}
				},
				{ctor: '[]'}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$input,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$type_('password'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$placeholder('Password'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Events$onInput(_user$project$LoginForm$Password),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$button,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onClick(_user$project$LoginForm$Login),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('Login'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$a,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$href(
									A2(_user$project$LoginForm$googleOauthUrl, model.host, model.googleClientId)),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('Google Login'),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};

var _user$project$UserInfo$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(
						A2(
							_elm_lang$core$Maybe$withDefault,
							'no user',
							A2(
								_elm_lang$core$Maybe$map,
								function (ui) {
									return A2(
										_elm_lang$core$Basics_ops['++'],
										ui.id,
										A2(_elm_lang$core$Basics_ops['++'], ' ', ui.username));
								},
								model.userInfo))),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _user$project$UserInfo$userInfo = function (model) {
	return model.userInfo;
};
var _user$project$UserInfo$emptyModel = {token: '', userInfo: _elm_lang$core$Maybe$Nothing};
var _user$project$UserInfo$Model = F2(
	function (a, b) {
		return {token: a, userInfo: b};
	});
var _user$project$UserInfo$FetchResult = function (a) {
	return {ctor: 'FetchResult', _0: a};
};
var _user$project$UserInfo$update = F2(
	function (msg, model) {
		var _p0 = msg;
		if (_p0.ctor === 'Fetch') {
			return {
				ctor: '_Tuple2',
				_0: model,
				_1: A2(_user$project$Api$fetchUserInfo, model.token, _user$project$UserInfo$FetchResult)
			};
		} else {
			if (_p0._0.ctor === 'Ok') {
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							userInfo: _elm_lang$core$Maybe$Just(_p0._0._0)
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			} else {
				return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
			}
		}
	});
var _user$project$UserInfo$Fetch = {ctor: 'Fetch'};
var _user$project$UserInfo$init = function (maybeToken) {
	var _p1 = maybeToken;
	if (_p1.ctor === 'Just') {
		var model = {token: _p1._0, userInfo: _elm_lang$core$Maybe$Nothing};
		return A2(_user$project$UserInfo$update, _user$project$UserInfo$Fetch, model);
	} else {
		return {ctor: '_Tuple2', _0: _user$project$UserInfo$emptyModel, _1: _elm_lang$core$Platform_Cmd$none};
	}
};

var _user$project$Main$toAuthToken = function (model) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		'no token',
		_user$project$LoginForm$token(model.loginFormModel));
};
var _user$project$Main$header = function (model) {
	return {
		ctor: '::',
		_0: A2(
			_debois$elm_mdl$Material_Layout$row,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$nop,
				_1: {
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Options$css, 'transition', 'height 333ms ease-in-out 0s'),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Layout$title,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('Receipts'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Layout$spacer,
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Layout$navigation,
							{ctor: '[]'},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					}
				}
			}),
		_1: {ctor: '[]'}
	};
};
var _user$project$Main$e404 = function (_p0) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A3(
				_debois$elm_mdl$Material_Options$styled,
				_elm_lang$html$Html$h1,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-typography--display-4'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Typography$center,
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('404'),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _user$project$Main$urlUpdate = F2(
	function (url, model) {
		return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
	});
var _user$project$Main$emptyPersistedModel = {token: _elm_lang$core$Maybe$Nothing};
var _user$project$Main$toPersistedModel = function (model) {
	return {
		token: _user$project$LoginForm$token(model.loginFormModel)
	};
};
var _user$project$Main$fromLocation = function (location) {
	return {
		host: A2(
			_elm_lang$core$Basics_ops['++'],
			location.protocol,
			A2(_elm_lang$core$Basics_ops['++'], '//', location.host)),
		hash: location.hash
	};
};
var _user$project$Main$setStorage = _elm_lang$core$Native_Platform.outgoingPort(
	'setStorage',
	function (v) {
		return {
			token: (v.token.ctor === 'Nothing') ? null : v.token._0
		};
	});
var _user$project$Main$withSetStorage = function (_p1) {
	var _p2 = _p1;
	var _p3 = _p2._0;
	return {
		ctor: '_Tuple2',
		_0: _p3,
		_1: _elm_lang$core$Platform_Cmd$batch(
			{
				ctor: '::',
				_0: _user$project$Main$setStorage(
					_user$project$Main$toPersistedModel(_p3)),
				_1: {
					ctor: '::',
					_0: _p2._1,
					_1: {ctor: '[]'}
				}
			})
	};
};
var _user$project$Main$ParsedLocation = F2(
	function (a, b) {
		return {host: a, hash: b};
	});
var _user$project$Main$PersistedModel = function (a) {
	return {token: a};
};
var _user$project$Main$Model = F5(
	function (a, b, c, d, e) {
		return {activePage: a, loginFormModel: b, userInfoModel: c, maybeAuthenticatedUserViewModel: d, mdl: e};
	});
var _user$project$Main$LoadingPage = {ctor: 'LoadingPage'};
var _user$project$Main$ReceiptListPage = {ctor: 'ReceiptListPage'};
var _user$project$Main$LoginPage = {ctor: 'LoginPage'};
var _user$project$Main$authTokenToPage = function (maybeAuthToken) {
	var _p4 = maybeAuthToken;
	if (_p4.ctor === 'Just') {
		return _user$project$Main$ReceiptListPage;
	} else {
		return _user$project$Main$LoginPage;
	}
};
var _user$project$Main$Mdl = function (a) {
	return {ctor: 'Mdl', _0: a};
};
var _user$project$Main$AuthenticatedUserViewMsg = function (a) {
	return {ctor: 'AuthenticatedUserViewMsg', _0: a};
};
var _user$project$Main$drawer = function (model) {
	var _p5 = model.maybeAuthenticatedUserViewModel;
	if (_p5.ctor === 'Just') {
		return {
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_Layout$title,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Receipts'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Layout$navigation,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$map,
							_user$project$Main$AuthenticatedUserViewMsg,
							_user$project$AuthenticatedUserView$drawerView(_p5._0)),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		};
	} else {
		return {ctor: '[]'};
	}
};
var _user$project$Main$subscriptions = function (model) {
	var _p6 = model.maybeAuthenticatedUserViewModel;
	if (_p6.ctor === 'Just') {
		return A2(
			_elm_lang$core$Platform_Sub$map,
			_user$project$Main$AuthenticatedUserViewMsg,
			_user$project$AuthenticatedUserView$subscriptions(_p6._0));
	} else {
		return _elm_lang$core$Platform_Sub$none;
	}
};
var _user$project$Main$UserInfoMsg = function (a) {
	return {ctor: 'UserInfoMsg', _0: a};
};
var _user$project$Main$LoginFormMsg = function (a) {
	return {ctor: 'LoginFormMsg', _0: a};
};
var _user$project$Main$init = F2(
	function (maybePersistedModel, location) {
		var parsedLocation = _user$project$Main$fromLocation(location);
		var persistedModel = A2(_elm_lang$core$Maybe$withDefault, _user$project$Main$emptyPersistedModel, maybePersistedModel);
		var _p7 = A3(_user$project$LoginForm$init, persistedModel.token, parsedLocation.host, parsedLocation.hash);
		var loginFormModel = _p7._0;
		var loginFormCmd = _p7._1;
		var _p8 = _user$project$UserInfo$init(persistedModel.token);
		var userInfoModel = _p8._0;
		var userInfoCmd = _p8._1;
		return {
			ctor: '_Tuple2',
			_0: {
				activePage: _user$project$Main$authTokenToPage(persistedModel.token),
				loginFormModel: loginFormModel,
				userInfoModel: userInfoModel,
				maybeAuthenticatedUserViewModel: _elm_lang$core$Maybe$Nothing,
				mdl: _debois$elm_mdl$Material$model
			},
			_1: _elm_lang$core$Platform_Cmd$batch(
				{
					ctor: '::',
					_0: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Main$LoginFormMsg, loginFormCmd),
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Main$UserInfoMsg, userInfoCmd),
						_1: {ctor: '[]'}
					}
				})
		};
	});
var _user$project$Main$update = F2(
	function (msg, model) {
		var _p9 = msg;
		switch (_p9.ctor) {
			case 'UrlChange':
				return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
			case 'Mdl':
				return A3(_debois$elm_mdl$Material$update, _user$project$Main$Mdl, _p9._0, model);
			case 'LoginFormMsg':
				var _p10 = A2(_user$project$LoginForm$update, _p9._0, model.loginFormModel);
				var loginFormModel = _p10._0;
				var loginFormCmd = _p10._1;
				var _p11 = {
					ctor: '_Tuple2',
					_0: _user$project$LoginForm$token(model.loginFormModel),
					_1: _user$project$LoginForm$token(loginFormModel)
				};
				if (((_p11.ctor === '_Tuple2') && (_p11._0.ctor === 'Nothing')) && (_p11._1.ctor === 'Just')) {
					var _p12 = _user$project$UserInfo$init(
						_elm_lang$core$Maybe$Just(_p11._1._0));
					var userInfoModel = _p12._0;
					var userInfoCmd = _p12._1;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								loginFormModel: loginFormModel,
								userInfoModel: userInfoModel,
								activePage: _user$project$Main$authTokenToPage(
									_user$project$LoginForm$token(loginFormModel))
							}),
						_1: _elm_lang$core$Platform_Cmd$batch(
							{
								ctor: '::',
								_0: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Main$LoginFormMsg, loginFormCmd),
								_1: {
									ctor: '::',
									_0: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Main$UserInfoMsg, userInfoCmd),
									_1: {ctor: '[]'}
								}
							})
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								loginFormModel: loginFormModel,
								activePage: _user$project$Main$authTokenToPage(
									_user$project$LoginForm$token(loginFormModel))
							}),
						_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Main$LoginFormMsg, loginFormCmd)
					};
				}
			case 'UserInfoMsg':
				var _p13 = A2(_user$project$UserInfo$update, _p9._0, model.userInfoModel);
				var userInfoModel = _p13._0;
				var userInfoCmd = _p13._1;
				var _p14 = {
					ctor: '_Tuple3',
					_0: _user$project$LoginForm$token(model.loginFormModel),
					_1: A2(
						_elm_lang$core$Maybe$map,
						function (ui) {
							return ui.id;
						},
						_user$project$UserInfo$userInfo(userInfoModel)),
					_2: _user$project$UserInfo$userInfo(model.userInfoModel)
				};
				if ((((_p14.ctor === '_Tuple3') && (_p14._0.ctor === 'Just')) && (_p14._1.ctor === 'Just')) && (_p14._2.ctor === 'Nothing')) {
					var _p15 = _user$project$AuthenticatedUserView$init(
						{userId: _p14._1._0, token: _p14._0._0});
					var authenticatedUserViewModel = _p15._0;
					var authenticatedUserViewCmd = _p15._1;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								userInfoModel: userInfoModel,
								maybeAuthenticatedUserViewModel: _elm_lang$core$Maybe$Just(authenticatedUserViewModel)
							}),
						_1: _elm_lang$core$Platform_Cmd$batch(
							{
								ctor: '::',
								_0: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Main$UserInfoMsg, userInfoCmd),
								_1: {
									ctor: '::',
									_0: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Main$AuthenticatedUserViewMsg, authenticatedUserViewCmd),
									_1: {ctor: '[]'}
								}
							})
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{userInfoModel: userInfoModel}),
						_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Main$UserInfoMsg, userInfoCmd)
					};
				}
			default:
				var _p16 = model.maybeAuthenticatedUserViewModel;
				if (_p16.ctor === 'Just') {
					var _p17 = A2(_user$project$AuthenticatedUserView$update, _p9._0, _p16._0);
					var authenticatedUserViewModel = _p17._0;
					var authenticatedUserViewCmd = _p17._1;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								maybeAuthenticatedUserViewModel: _elm_lang$core$Maybe$Just(authenticatedUserViewModel)
							}),
						_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Main$AuthenticatedUserViewMsg, authenticatedUserViewCmd)
					};
				} else {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
		}
	});
var _user$project$Main$pageView = function (model) {
	var _p18 = model.activePage;
	switch (_p18.ctor) {
		case 'LoginPage':
			return A2(
				_elm_lang$html$Html$map,
				_user$project$Main$LoginFormMsg,
				_user$project$LoginForm$view(model.loginFormModel));
		case 'ReceiptListPage':
			var _p19 = model.maybeAuthenticatedUserViewModel;
			if (_p19.ctor === 'Just') {
				return A2(
					_elm_lang$html$Html$map,
					_user$project$Main$AuthenticatedUserViewMsg,
					_user$project$AuthenticatedUserView$view(_p19._0));
			} else {
				return A2(
					_elm_lang$html$Html$div,
					{ctor: '[]'},
					{ctor: '[]'});
			}
		default:
			return A2(
				_elm_lang$html$Html$span,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Loading page'),
					_1: {ctor: '[]'}
				});
	}
};
var _user$project$Main$view = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A4(
				_debois$elm_mdl$Material_Layout$render,
				_user$project$Main$Mdl,
				model.mdl,
				{ctor: '[]'},
				{
					header: _user$project$Main$header(model),
					drawer: _user$project$Main$drawer(model),
					tabs: {
						ctor: '_Tuple2',
						_0: {ctor: '[]'},
						_1: {ctor: '[]'}
					},
					main: {
						ctor: '::',
						_0: _user$project$Main$pageView(model),
						_1: {ctor: '[]'}
					}
				}),
			_1: {ctor: '[]'}
		});
};
var _user$project$Main$UrlChange = function (a) {
	return {ctor: 'UrlChange', _0: a};
};
var _user$project$Main$main = A2(
	_elm_lang$navigation$Navigation$programWithFlags,
	_user$project$Main$UrlChange,
	{
		init: _user$project$Main$init,
		view: _user$project$Main$view,
		update: F2(
			function (msg, model) {
				return _user$project$Main$withSetStorage(
					A2(_user$project$Main$update, msg, model));
			}),
		subscriptions: _user$project$Main$subscriptions
	})(
	_elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$core$Json_Decode$map,
					_elm_lang$core$Maybe$Just,
					A2(
						_elm_lang$core$Json_Decode$andThen,
						function (token) {
							return _elm_lang$core$Json_Decode$succeed(
								{token: token});
						},
						A2(
							_elm_lang$core$Json_Decode$field,
							'token',
							_elm_lang$core$Json_Decode$oneOf(
								{
									ctor: '::',
									_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
									_1: {
										ctor: '::',
										_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _elm_lang$core$Json_Decode$string),
										_1: {ctor: '[]'}
									}
								})))),
				_1: {ctor: '[]'}
			}
		}));

var Elm = {};
Elm['Main'] = Elm['Main'] || {};
if (typeof _user$project$Main$main !== 'undefined') {
    _user$project$Main$main(Elm['Main'], 'Main', {"types":{"unions":{"AddReceiptForm.Msg":{"args":[],"tags":{"CurrentTime":["Time.Time"],"UploadReceipt":[],"ReceiptFileChange":["Ports.FileToUpload"],"ReceiptFileInputStart":[],"ReceiptUploaded":["Ports.CreateReceiptResult"]}},"LoginForm.Msg":{"args":[],"tags":{"LoginWithGoogle":["String"],"AppConfigFetchResult":["Result.Result Api.Error Models.AppConfig"],"AppConfigFetch":[],"Name":["String"],"Password":["String"],"LoginResult":["Result.Result Api.Error String"],"Login":[]}},"Material.Component.Msg":{"args":["button","textfield","menu","layout","toggles","tooltip","tabs","dispatch"],"tags":{"TooltipMsg":["Material.Component.Index","tooltip"],"TogglesMsg":["Material.Component.Index","toggles"],"LayoutMsg":["layout"],"ButtonMsg":["Material.Component.Index","button"],"MenuMsg":["Material.Component.Index","menu"],"TabsMsg":["Material.Component.Index","tabs"],"Dispatch":["dispatch"],"TextfieldMsg":["Material.Component.Index","textfield"]}},"DatePicker.Msg":{"args":[],"tags":{"SelectDay":["Int"],"DaySelection":[],"NextMonth":[],"SelectYear":["Int"],"YearSelection":[],"Noop":[],"PrevMonth":[]}},"DateTimePicker.Msg":{"args":[],"tags":{"TimePickerMsg":["TimePicker.Msg"],"DatePickerMsg":["DatePicker.Msg"]}},"Material.Ripple.Msg":{"args":[],"tags":{"Down":["Material.Ripple.DOMState"],"Up":[],"Tick":[]}},"TimeTypes.TimePeriod":{"args":[],"tags":{"PM":[],"AM":[]}},"TimeTypes.Mode":{"args":[],"tags":{"Hours":[],"Minutes":[]}},"Maybe.Maybe":{"args":["a"],"tags":{"Just":["a"],"Nothing":[]}},"Backup.Msg":{"args":[],"tags":{"DownloadBackup":[],"BackupUrlResult":["Result.Result Api.Error String"],"Mdl":["Material.Msg Backup.Msg"]}},"TimePicker.Msg":{"args":[],"tags":{"MouseUp":["MousePosition.Offset"],"TimePeriodSwitch":["TimeTypes.TimePeriod"],"MouseDown":["MousePosition.Offset"],"ModeSwitch":["TimeTypes.Mode"],"MouseMove":["MousePosition.Offset"],"Noop":[]}},"Main.Msg":{"args":[],"tags":{"LoginFormMsg":["LoginForm.Msg"],"UserInfoMsg":["UserInfo.Msg"],"AuthenticatedUserViewMsg":["AuthenticatedUserView.Msg"],"UrlChange":["Navigation.Location"],"Mdl":["Material.Msg Main.Msg"]}},"Material.Tooltip.Msg":{"args":[],"tags":{"Enter":["Material.Tooltip.DOMState"],"Leave":[]}},"AuthenticatedUserView.Msg":{"args":[],"tags":{"ReceiptListMsg":["ReceiptList.Msg"],"AddReceiptFormMsg":["AddReceiptForm.Msg"],"ShowNewReceiptForm":[],"HideNewReceiptForm":[],"BackupMsg":["Backup.Msg"],"Mdl":["Material.Msg AuthenticatedUserView.Msg"]}},"Json.Decode.Decoder":{"args":["a"],"tags":{"Decoder":[]}},"Material.Textfield.Msg":{"args":[],"tags":{"Focus":[],"Input":["String"],"Blur":[]}},"UserInfo.Msg":{"args":[],"tags":{"Fetch":[],"FetchResult":["Result.Result Api.Error Models.UserInfo"]}},"Material.Layout.Msg":{"args":[],"tags":{"Resize":["Int"],"ToggleDrawer":[],"TransitionEnd":[],"ScrollPane":["Bool","Float"],"Ripple":["Int","Material.Ripple.Msg"],"ScrollTab":["Material.Layout.TabScrollState"],"TransitionHeader":["{ toCompact : Bool, fixedHeader : Bool }"],"NOP":[]}},"Material.Toggles.Msg":{"args":[],"tags":{"Ripple":["Material.Ripple.Msg"],"SetFocus":["Bool"]}},"VirtualDom.Property":{"args":["msg"],"tags":{"Property":[]}},"ReceiptView.Msg":{"args":[],"tags":{"MouseUp":["MousePosition.Offset"],"UpdateReceiptResult":["Result.Result Api.Error Models.Receipt"],"TotalChange":["String"],"UpdateReceipt":[],"EditModeSwitch":[],"MouseDown":["MousePosition.Offset"],"SetImageUrl":[],"CloseReceipt":[],"Mdl":["Material.Msg ReceiptView.Msg"],"MouseMove":["MousePosition.Offset"],"DescriptionChange":["String"],"DateTimePickerMsg":["DateTimePicker.Msg"]}},"Result.Result":{"args":["error","value"],"tags":{"Ok":["value"],"Err":["error"]}},"Api.Error":{"args":[],"tags":{"Error":["String"]}},"Material.Tabs.Msg":{"args":[],"tags":{"Ripple":["Int","Material.Ripple.Msg"]}},"Material.Menu.Msg":{"args":["m"],"tags":{"Tick":[],"Close":[],"Open":["Material.Menu.Geometry.Geometry"],"Key":["List (Material.Options.Internal.Summary (Material.Menu.ItemConfig m) m)","Int"],"Ripple":["Int","Material.Ripple.Msg"],"Select":["Int","Maybe.Maybe m"],"Click":["Mouse.Position"]}},"Material.Dispatch.Config":{"args":["msg"],"tags":{"Config":["{ decoders : List ( String , ( Json.Decode.Decoder msg, Maybe.Maybe Html.Events.Options ) ) , lift : Maybe.Maybe (Json.Decode.Decoder (List msg) -> Json.Decode.Decoder msg) }"]}},"ReceiptList.Msg":{"args":[],"tags":{"Fetch":[],"OpenReceiptView":["Models.Receipt"],"FetchResult":["Result.Result Api.Error (List Models.Receipt)"],"ReceiptViewMsg":["ReceiptView.Msg"]}}},"aliases":{"Material.Button.Msg":{"args":[],"type":"Material.Ripple.Msg"},"Material.Layout.TabScrollState":{"args":[],"type":"{ canScrollLeft : Bool , canScrollRight : Bool , width : Maybe.Maybe Int }"},"Material.Tooltip.DOMState":{"args":[],"type":"{ rect : DOM.Rectangle, offsetWidth : Float, offsetHeight : Float }"},"Html.Attribute":{"args":["msg"],"type":"VirtualDom.Property msg"},"Material.Menu.ItemConfig":{"args":["m"],"type":"{ enabled : Bool, divider : Bool, onSelect : Maybe.Maybe m }"},"Material.Component.Index":{"args":[],"type":"List Int"},"Html.Events.Options":{"args":[],"type":"{ stopPropagation : Bool, preventDefault : Bool }"},"Models.UserInfo":{"args":[],"type":"{ id : String, username : String }"},"Material.Ripple.DOMState":{"args":[],"type":"{ rect : DOM.Rectangle , clientX : Maybe.Maybe Float , clientY : Maybe.Maybe Float , touchX : Maybe.Maybe Float , touchY : Maybe.Maybe Float , type_ : String }"},"Models.ReceiptFile":{"args":[],"type":"{ id : String , ext : String , metaData : Models.FileMetadata , timestamp : Int }"},"MousePosition.Offset":{"args":[],"type":"{ x : Int, y : Int }"},"Mouse.Position":{"args":[],"type":"{ x : Int, y : Int }"},"Material.Options.Internal.Summary":{"args":["c","m"],"type":"{ classes : List String , css : List ( String, String ) , attrs : List (Html.Attribute m) , internal : List (Html.Attribute m) , dispatch : Material.Dispatch.Config m , config : c }"},"Models.AppConfig":{"args":[],"type":"{ googleClientId : String }"},"Material.Msg":{"args":["m"],"type":"Material.Component.Msg Material.Button.Msg Material.Textfield.Msg (Material.Menu.Msg m) Material.Layout.Msg Material.Toggles.Msg Material.Tooltip.Msg Material.Tabs.Msg (List m)"},"MousePosition.Target":{"args":[],"type":"{ offsetWidth : Int, offsetHeight : Int }"},"Ports.FileToUpload":{"args":[],"type":"{ isImage : Bool, imageDataUrl : Maybe.Maybe String }"},"Material.Menu.Geometry.Element":{"args":[],"type":"{ offsetTop : Float , offsetLeft : Float , offsetHeight : Float , bounds : DOM.Rectangle }"},"Ports.CreateReceiptResult":{"args":[],"type":"{ receiptId : String, error : Maybe.Maybe String }"},"Material.Menu.Geometry.Geometry":{"args":[],"type":"{ button : Material.Menu.Geometry.Element , menu : Material.Menu.Geometry.Element , container : Material.Menu.Geometry.Element , offsetTops : List Float , offsetHeights : List Float }"},"Time.Time":{"args":[],"type":"Float"},"Models.Receipt":{"args":[],"type":"{ id : String , userId : String , files : List Models.ReceiptFile , timestamp : Int , transactionTime : Int , total : Maybe.Maybe Float , description : String , tags : List String }"},"Navigation.Location":{"args":[],"type":"{ href : String , host : String , hostname : String , protocol : String , origin : String , port_ : String , pathname : String , search : String , hash : String , username : String , password : String }"},"Models.FileMetadata":{"args":[],"type":"{ fileType : String, length : Int, width : Int, height : Int }"},"DOM.Rectangle":{"args":[],"type":"{ top : Float, left : Float, width : Float, height : Float }"}},"message":"Main.Msg"},"versions":{"elm":"0.18.0"}});
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

