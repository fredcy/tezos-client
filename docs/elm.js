
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

var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode = _elm_lang$core$Json_Decode$succeed;
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$resolve = _elm_lang$core$Json_Decode$andThen(_elm_lang$core$Basics$identity);
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom = _elm_lang$core$Json_Decode$map2(
	F2(
		function (x, y) {
			return y(x);
		}));
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$hardcoded = function (_p0) {
	return _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom(
		_elm_lang$core$Json_Decode$succeed(_p0));
};
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder = F3(
	function (pathDecoder, valDecoder, fallback) {
		var nullOr = function (decoder) {
			return _elm_lang$core$Json_Decode$oneOf(
				{
					ctor: '::',
					_0: decoder,
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Json_Decode$null(fallback),
						_1: {ctor: '[]'}
					}
				});
		};
		var handleResult = function (input) {
			var _p1 = A2(_elm_lang$core$Json_Decode$decodeValue, pathDecoder, input);
			if (_p1.ctor === 'Ok') {
				var _p2 = A2(
					_elm_lang$core$Json_Decode$decodeValue,
					nullOr(valDecoder),
					_p1._0);
				if (_p2.ctor === 'Ok') {
					return _elm_lang$core$Json_Decode$succeed(_p2._0);
				} else {
					return _elm_lang$core$Json_Decode$fail(_p2._0);
				}
			} else {
				return _elm_lang$core$Json_Decode$succeed(fallback);
			}
		};
		return A2(_elm_lang$core$Json_Decode$andThen, handleResult, _elm_lang$core$Json_Decode$value);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalAt = F4(
	function (path, valDecoder, fallback, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder,
				A2(_elm_lang$core$Json_Decode$at, path, _elm_lang$core$Json_Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional = F4(
	function (key, valDecoder, fallback, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder,
				A2(_elm_lang$core$Json_Decode$field, key, _elm_lang$core$Json_Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt = F3(
	function (path, valDecoder, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A2(_elm_lang$core$Json_Decode$at, path, valDecoder),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A2(_elm_lang$core$Json_Decode$field, key, valDecoder),
			decoder);
	});

var _alpacaaa$elm_date_distance$Date_Distance_Types$Config = F2(
	function (a, b) {
		return {locale: a, includeSeconds: b};
	});
var _alpacaaa$elm_date_distance$Date_Distance_Types$AlmostXYears = function (a) {
	return {ctor: 'AlmostXYears', _0: a};
};
var _alpacaaa$elm_date_distance$Date_Distance_Types$OverXYears = function (a) {
	return {ctor: 'OverXYears', _0: a};
};
var _alpacaaa$elm_date_distance$Date_Distance_Types$AboutXYears = function (a) {
	return {ctor: 'AboutXYears', _0: a};
};
var _alpacaaa$elm_date_distance$Date_Distance_Types$XMonths = function (a) {
	return {ctor: 'XMonths', _0: a};
};
var _alpacaaa$elm_date_distance$Date_Distance_Types$AboutXMonths = function (a) {
	return {ctor: 'AboutXMonths', _0: a};
};
var _alpacaaa$elm_date_distance$Date_Distance_Types$XDays = function (a) {
	return {ctor: 'XDays', _0: a};
};
var _alpacaaa$elm_date_distance$Date_Distance_Types$AboutXHours = function (a) {
	return {ctor: 'AboutXHours', _0: a};
};
var _alpacaaa$elm_date_distance$Date_Distance_Types$XMinutes = function (a) {
	return {ctor: 'XMinutes', _0: a};
};
var _alpacaaa$elm_date_distance$Date_Distance_Types$LessThanXMinutes = function (a) {
	return {ctor: 'LessThanXMinutes', _0: a};
};
var _alpacaaa$elm_date_distance$Date_Distance_Types$HalfAMinute = {ctor: 'HalfAMinute'};
var _alpacaaa$elm_date_distance$Date_Distance_Types$LessThanXSeconds = function (a) {
	return {ctor: 'LessThanXSeconds', _0: a};
};

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

var _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerSecond = 1000;
var _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute = 60 * _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerSecond;
var _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerHour = 60 * _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute;
var _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerDay = 24 * _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerHour;
var _justinmimbs$elm_date_extra$Date_Extra_Facts$dayOfWeekFromWeekdayNumber = function (n) {
	var _p0 = n;
	switch (_p0) {
		case 1:
			return _elm_lang$core$Date$Mon;
		case 2:
			return _elm_lang$core$Date$Tue;
		case 3:
			return _elm_lang$core$Date$Wed;
		case 4:
			return _elm_lang$core$Date$Thu;
		case 5:
			return _elm_lang$core$Date$Fri;
		case 6:
			return _elm_lang$core$Date$Sat;
		default:
			return _elm_lang$core$Date$Sun;
	}
};
var _justinmimbs$elm_date_extra$Date_Extra_Facts$weekdayNumberFromDayOfWeek = function (d) {
	var _p1 = d;
	switch (_p1.ctor) {
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
var _justinmimbs$elm_date_extra$Date_Extra_Facts$monthFromMonthNumber = function (n) {
	var _p2 = n;
	switch (_p2) {
		case 1:
			return _elm_lang$core$Date$Jan;
		case 2:
			return _elm_lang$core$Date$Feb;
		case 3:
			return _elm_lang$core$Date$Mar;
		case 4:
			return _elm_lang$core$Date$Apr;
		case 5:
			return _elm_lang$core$Date$May;
		case 6:
			return _elm_lang$core$Date$Jun;
		case 7:
			return _elm_lang$core$Date$Jul;
		case 8:
			return _elm_lang$core$Date$Aug;
		case 9:
			return _elm_lang$core$Date$Sep;
		case 10:
			return _elm_lang$core$Date$Oct;
		case 11:
			return _elm_lang$core$Date$Nov;
		default:
			return _elm_lang$core$Date$Dec;
	}
};
var _justinmimbs$elm_date_extra$Date_Extra_Facts$monthNumberFromMonth = function (m) {
	var _p3 = m;
	switch (_p3.ctor) {
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
var _justinmimbs$elm_date_extra$Date_Extra_Facts$months = {
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
var _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear = function (y) {
	return (_elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], y, 4),
		0) && (!_elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], y, 100),
		0))) || _elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], y, 400),
		0);
};
var _justinmimbs$elm_date_extra$Date_Extra_Facts$daysInMonth = F2(
	function (y, m) {
		var _p4 = m;
		switch (_p4.ctor) {
			case 'Jan':
				return 31;
			case 'Feb':
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 29 : 28;
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
var _justinmimbs$elm_date_extra$Date_Extra_Facts$daysBeforeStartOfMonth = F2(
	function (y, m) {
		var _p5 = m;
		switch (_p5.ctor) {
			case 'Jan':
				return 0;
			case 'Feb':
				return 31;
			case 'Mar':
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 60 : 59;
			case 'Apr':
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 91 : 90;
			case 'May':
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 121 : 120;
			case 'Jun':
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 152 : 151;
			case 'Jul':
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 182 : 181;
			case 'Aug':
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 213 : 212;
			case 'Sep':
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 244 : 243;
			case 'Oct':
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 274 : 273;
			case 'Nov':
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 305 : 304;
			default:
				return _justinmimbs$elm_date_extra$Date_Extra_Facts$isLeapYear(y) ? 335 : 334;
		}
	});

var _justinmimbs$elm_date_extra$Date_Internal_RataDie$toUnixTime = function (rd) {
	return (rd - 719163) * _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerDay;
};
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$weekdayNumber = function (rd) {
	var _p0 = A2(_elm_lang$core$Basics_ops['%'], rd, 7);
	if (_p0 === 0) {
		return 7;
	} else {
		return _p0;
	}
};
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$leapYearsInCommonEra = function (y) {
	return (((y / 4) | 0) - ((y / 100) | 0)) + ((y / 400) | 0);
};
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$rataDieBeforeStartOfYear = function (y) {
	return (365 * (y - 1)) + _justinmimbs$elm_date_extra$Date_Internal_RataDie$leapYearsInCommonEra(y - 1);
};
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$fromOrdinalDate = F2(
	function (y, d) {
		return _justinmimbs$elm_date_extra$Date_Internal_RataDie$rataDieBeforeStartOfYear(y) + d;
	});
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$week1Day1OfWeekYear = function (y) {
	var jan4RD = A2(_justinmimbs$elm_date_extra$Date_Internal_RataDie$fromOrdinalDate, y, 4);
	return (jan4RD - _justinmimbs$elm_date_extra$Date_Internal_RataDie$weekdayNumber(jan4RD)) + 1;
};
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$fromWeekDate = F3(
	function (y, w, d) {
		var week1Day0RD = _justinmimbs$elm_date_extra$Date_Internal_RataDie$week1Day1OfWeekYear(y) - 1;
		return (week1Day0RD + ((w - 1) * 7)) + d;
	});
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$fromCalendarDate = F3(
	function (y, m, d) {
		var md = A2(_justinmimbs$elm_date_extra$Date_Extra_Facts$daysBeforeStartOfMonth, y, m);
		var yd = _justinmimbs$elm_date_extra$Date_Internal_RataDie$rataDieBeforeStartOfYear(y);
		return (yd + md) + d;
	});
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$divideInt = F2(
	function (a, b) {
		return {
			ctor: '_Tuple2',
			_0: (a / b) | 0,
			_1: A2(_elm_lang$core$Basics$rem, a, b)
		};
	});
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$year = function (rd) {
	var _p1 = A2(_justinmimbs$elm_date_extra$Date_Internal_RataDie$divideInt, rd, 146097);
	var q400 = _p1._0;
	var r400 = _p1._1;
	var _p2 = A2(_justinmimbs$elm_date_extra$Date_Internal_RataDie$divideInt, r400, 36524);
	var q100 = _p2._0;
	var r100 = _p2._1;
	var _p3 = A2(_justinmimbs$elm_date_extra$Date_Internal_RataDie$divideInt, r100, 1461);
	var q4 = _p3._0;
	var r4 = _p3._1;
	var _p4 = A2(_justinmimbs$elm_date_extra$Date_Internal_RataDie$divideInt, r4, 365);
	var q1 = _p4._0;
	var r1 = _p4._1;
	var n = _elm_lang$core$Native_Utils.eq(r1, 0) ? 0 : 1;
	return ((((q400 * 400) + (q100 * 100)) + (q4 * 4)) + q1) + n;
};
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$ordinalDay = function (rd) {
	return rd - _justinmimbs$elm_date_extra$Date_Internal_RataDie$rataDieBeforeStartOfYear(
		_justinmimbs$elm_date_extra$Date_Internal_RataDie$year(rd));
};
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$weekYear = function (rd) {
	var daysToThursday = 4 - _justinmimbs$elm_date_extra$Date_Internal_RataDie$weekdayNumber(rd);
	return _justinmimbs$elm_date_extra$Date_Internal_RataDie$year(rd + daysToThursday);
};
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$weekNumber = function (rd) {
	var week1Day1RD = _justinmimbs$elm_date_extra$Date_Internal_RataDie$week1Day1OfWeekYear(
		_justinmimbs$elm_date_extra$Date_Internal_RataDie$weekYear(rd));
	return (((rd - week1Day1RD) / 7) | 0) + 1;
};
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$find = F2(
	function (pred, list) {
		find:
		while (true) {
			var _p5 = list;
			if (_p5.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p6 = _p5._0;
				if (pred(_p6)) {
					return _elm_lang$core$Maybe$Just(_p6);
				} else {
					var _v2 = pred,
						_v3 = _p5._1;
					pred = _v2;
					list = _v3;
					continue find;
				}
			}
		}
	});
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$month = function (rd) {
	var od = _justinmimbs$elm_date_extra$Date_Internal_RataDie$ordinalDay(rd);
	var y = _justinmimbs$elm_date_extra$Date_Internal_RataDie$year(rd);
	return A2(
		_elm_lang$core$Maybe$withDefault,
		_elm_lang$core$Date$Jan,
		A2(
			_justinmimbs$elm_date_extra$Date_Internal_RataDie$find,
			function (m) {
				return _elm_lang$core$Native_Utils.cmp(
					A2(_justinmimbs$elm_date_extra$Date_Extra_Facts$daysBeforeStartOfMonth, y, m),
					od) < 0;
			},
			_elm_lang$core$List$reverse(_justinmimbs$elm_date_extra$Date_Extra_Facts$months)));
};
var _justinmimbs$elm_date_extra$Date_Internal_RataDie$day = function (rd) {
	var od = _justinmimbs$elm_date_extra$Date_Internal_RataDie$ordinalDay(rd);
	var m = _justinmimbs$elm_date_extra$Date_Internal_RataDie$month(rd);
	var y = _justinmimbs$elm_date_extra$Date_Internal_RataDie$year(rd);
	return od - A2(_justinmimbs$elm_date_extra$Date_Extra_Facts$daysBeforeStartOfMonth, y, m);
};

var _justinmimbs$elm_date_extra$Date_Internal_Core$weekNumberFromCalendarDate = F3(
	function (y, m, d) {
		return _justinmimbs$elm_date_extra$Date_Internal_RataDie$weekNumber(
			A3(_justinmimbs$elm_date_extra$Date_Internal_RataDie$fromCalendarDate, y, m, d));
	});
var _justinmimbs$elm_date_extra$Date_Internal_Core$weekYearFromCalendarDate = F3(
	function (y, m, d) {
		return _justinmimbs$elm_date_extra$Date_Internal_RataDie$weekYear(
			A3(_justinmimbs$elm_date_extra$Date_Internal_RataDie$fromCalendarDate, y, m, d));
	});
var _justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromOrdinalDate = F2(
	function (y, d) {
		return _justinmimbs$elm_date_extra$Date_Internal_RataDie$toUnixTime(
			A2(_justinmimbs$elm_date_extra$Date_Internal_RataDie$fromOrdinalDate, y, d));
	});
var _justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromWeekDate = F3(
	function (y, w, d) {
		return _justinmimbs$elm_date_extra$Date_Internal_RataDie$toUnixTime(
			A3(_justinmimbs$elm_date_extra$Date_Internal_RataDie$fromWeekDate, y, w, d));
	});
var _justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromCalendarDate = F3(
	function (y, m, d) {
		return _justinmimbs$elm_date_extra$Date_Internal_RataDie$toUnixTime(
			A3(_justinmimbs$elm_date_extra$Date_Internal_RataDie$fromCalendarDate, y, m, d));
	});
var _justinmimbs$elm_date_extra$Date_Internal_Core$msFromTimeParts = F4(
	function (hh, mm, ss, ms) {
		return ((ms + (_justinmimbs$elm_date_extra$Date_Extra_Facts$msPerSecond * ss)) + (_justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute * mm)) + (_justinmimbs$elm_date_extra$Date_Extra_Facts$msPerHour * hh);
	});
var _justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromParts = F7(
	function (y, m, d, hh, mm, ss, ms) {
		return _justinmimbs$elm_date_extra$Date_Internal_RataDie$toUnixTime(
			A3(_justinmimbs$elm_date_extra$Date_Internal_RataDie$fromCalendarDate, y, m, d)) + A4(_justinmimbs$elm_date_extra$Date_Internal_Core$msFromTimeParts, hh, mm, ss, ms);
	});

var _justinmimbs$elm_date_extra$Date_Internal_Extract$msOffsetFromUtc = function (date) {
	var utcTime = _elm_lang$core$Date$toTime(date);
	var localTime = _elm_lang$core$Basics$toFloat(
		A7(
			_justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromParts,
			_elm_lang$core$Date$year(date),
			_elm_lang$core$Date$month(date),
			_elm_lang$core$Date$day(date),
			_elm_lang$core$Date$hour(date),
			_elm_lang$core$Date$minute(date),
			_elm_lang$core$Date$second(date),
			_elm_lang$core$Date$millisecond(date)));
	return _elm_lang$core$Basics$floor(localTime - utcTime);
};
var _justinmimbs$elm_date_extra$Date_Internal_Extract$offsetFromUtc = function (date) {
	return (_justinmimbs$elm_date_extra$Date_Internal_Extract$msOffsetFromUtc(date) / _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute) | 0;
};
var _justinmimbs$elm_date_extra$Date_Internal_Extract$weekYear = function (date) {
	return A3(
		_justinmimbs$elm_date_extra$Date_Internal_Core$weekYearFromCalendarDate,
		_elm_lang$core$Date$year(date),
		_elm_lang$core$Date$month(date),
		_elm_lang$core$Date$day(date));
};
var _justinmimbs$elm_date_extra$Date_Internal_Extract$weekNumber = function (date) {
	return A3(
		_justinmimbs$elm_date_extra$Date_Internal_Core$weekNumberFromCalendarDate,
		_elm_lang$core$Date$year(date),
		_elm_lang$core$Date$month(date),
		_elm_lang$core$Date$day(date));
};
var _justinmimbs$elm_date_extra$Date_Internal_Extract$weekdayNumber = function (_p0) {
	return _justinmimbs$elm_date_extra$Date_Extra_Facts$weekdayNumberFromDayOfWeek(
		_elm_lang$core$Date$dayOfWeek(_p0));
};
var _justinmimbs$elm_date_extra$Date_Internal_Extract$fractionalDay = function (date) {
	var timeOfDayMS = A4(
		_justinmimbs$elm_date_extra$Date_Internal_Core$msFromTimeParts,
		_elm_lang$core$Date$hour(date),
		_elm_lang$core$Date$minute(date),
		_elm_lang$core$Date$second(date),
		_elm_lang$core$Date$millisecond(date));
	return _elm_lang$core$Basics$toFloat(timeOfDayMS) / _elm_lang$core$Basics$toFloat(_justinmimbs$elm_date_extra$Date_Extra_Facts$msPerDay);
};
var _justinmimbs$elm_date_extra$Date_Internal_Extract$ordinalDay = function (date) {
	return A2(
		_justinmimbs$elm_date_extra$Date_Extra_Facts$daysBeforeStartOfMonth,
		_elm_lang$core$Date$year(date),
		_elm_lang$core$Date$month(date)) + _elm_lang$core$Date$day(date);
};
var _justinmimbs$elm_date_extra$Date_Internal_Extract$monthNumber = function (_p1) {
	return _justinmimbs$elm_date_extra$Date_Extra_Facts$monthNumberFromMonth(
		_elm_lang$core$Date$month(_p1));
};
var _justinmimbs$elm_date_extra$Date_Internal_Extract$quarter = function (date) {
	return _elm_lang$core$Basics$ceiling(
		function (n) {
			return n / 3;
		}(
			_elm_lang$core$Basics$toFloat(
				_justinmimbs$elm_date_extra$Date_Internal_Extract$monthNumber(date))));
};

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

var _justinmimbs$elm_date_extra$Date_Internal_Format$toUtc = function (date) {
	return _elm_lang$core$Date$fromTime(
		_elm_lang$core$Date$toTime(date) - _elm_lang$core$Basics$toFloat(
			_justinmimbs$elm_date_extra$Date_Internal_Extract$offsetFromUtc(date) * _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute));
};
var _justinmimbs$elm_date_extra$Date_Internal_Format$nameForm = function (length) {
	var _p0 = length;
	switch (_p0) {
		case 1:
			return 'abbreviated';
		case 2:
			return 'abbreviated';
		case 3:
			return 'abbreviated';
		case 4:
			return 'full';
		case 5:
			return 'narrow';
		case 6:
			return 'short';
		default:
			return 'invalid';
	}
};
var _justinmimbs$elm_date_extra$Date_Internal_Format$patternMatches = _elm_lang$core$Regex$regex('([yYQMwdDEeabhHmsSXx])\\1*|\'(?:[^\']|\'\')*?\'(?!\')');
var _justinmimbs$elm_date_extra$Date_Internal_Format$formatTimeOffset = F3(
	function (separator, minutesOptional, offset) {
		var mm = A3(
			_elm_lang$core$String$padLeft,
			2,
			_elm_lang$core$Native_Utils.chr('0'),
			_elm_lang$core$Basics$toString(
				A2(
					_elm_lang$core$Basics_ops['%'],
					_elm_lang$core$Basics$abs(offset),
					60)));
		var hh = A3(
			_elm_lang$core$String$padLeft,
			2,
			_elm_lang$core$Native_Utils.chr('0'),
			_elm_lang$core$Basics$toString(
				(_elm_lang$core$Basics$abs(offset) / 60) | 0));
		var sign = (_elm_lang$core$Native_Utils.cmp(offset, 0) > -1) ? '+' : '-';
		return (minutesOptional && _elm_lang$core$Native_Utils.eq(mm, '00')) ? A2(_elm_lang$core$Basics_ops['++'], sign, hh) : A2(
			_elm_lang$core$Basics_ops['++'],
			sign,
			A2(
				_elm_lang$core$Basics_ops['++'],
				hh,
				A2(_elm_lang$core$Basics_ops['++'], separator, mm)));
	});
var _justinmimbs$elm_date_extra$Date_Internal_Format$ordinalSuffix = function (n) {
	var nn = A2(_elm_lang$core$Basics_ops['%'], n, 100);
	var _p1 = A2(
		_elm_lang$core$Basics$min,
		(_elm_lang$core$Native_Utils.cmp(nn, 20) < 0) ? nn : A2(_elm_lang$core$Basics_ops['%'], nn, 10),
		4);
	switch (_p1) {
		case 0:
			return 'th';
		case 1:
			return 'st';
		case 2:
			return 'nd';
		case 3:
			return 'rd';
		case 4:
			return 'th';
		default:
			return '';
	}
};
var _justinmimbs$elm_date_extra$Date_Internal_Format$withOrdinalSuffix = function (n) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(n),
		_justinmimbs$elm_date_extra$Date_Internal_Format$ordinalSuffix(n));
};
var _justinmimbs$elm_date_extra$Date_Internal_Format$hour12 = function (date) {
	var _p2 = A2(
		_elm_lang$core$Basics_ops['%'],
		_elm_lang$core$Date$hour(date),
		12);
	if (_p2 === 0) {
		return 12;
	} else {
		return _p2;
	}
};
var _justinmimbs$elm_date_extra$Date_Internal_Format$dayOfWeekName = function (d) {
	var _p3 = d;
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
var _justinmimbs$elm_date_extra$Date_Internal_Format$monthName = function (m) {
	var _p4 = m;
	switch (_p4.ctor) {
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
var _justinmimbs$elm_date_extra$Date_Internal_Format$PM = {ctor: 'PM'};
var _justinmimbs$elm_date_extra$Date_Internal_Format$Noon = {ctor: 'Noon'};
var _justinmimbs$elm_date_extra$Date_Internal_Format$AM = {ctor: 'AM'};
var _justinmimbs$elm_date_extra$Date_Internal_Format$Midnight = {ctor: 'Midnight'};
var _justinmimbs$elm_date_extra$Date_Internal_Format$dayPeriod = function (date) {
	var onTheHour = _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Date$minute(date),
		0) && (_elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Date$second(date),
		0) && _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Date$millisecond(date),
		0));
	var hh = _elm_lang$core$Date$hour(date);
	return (_elm_lang$core$Native_Utils.eq(hh, 0) && onTheHour) ? _justinmimbs$elm_date_extra$Date_Internal_Format$Midnight : ((_elm_lang$core$Native_Utils.cmp(hh, 12) < 0) ? _justinmimbs$elm_date_extra$Date_Internal_Format$AM : ((_elm_lang$core$Native_Utils.eq(hh, 12) && onTheHour) ? _justinmimbs$elm_date_extra$Date_Internal_Format$Noon : _justinmimbs$elm_date_extra$Date_Internal_Format$PM));
};
var _justinmimbs$elm_date_extra$Date_Internal_Format$format = F3(
	function (asUtc, date, match) {
		format:
		while (true) {
			var length = _elm_lang$core$String$length(match);
			var $char = A2(_elm_lang$core$String$left, 1, match);
			var _p5 = $char;
			switch (_p5) {
				case 'y':
					var _p6 = length;
					if (_p6 === 2) {
						return A2(
							_elm_lang$core$String$right,
							2,
							A3(
								_elm_lang$core$String$padLeft,
								length,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_elm_lang$core$Date$year(date))));
					} else {
						return A3(
							_elm_lang$core$String$padLeft,
							length,
							_elm_lang$core$Native_Utils.chr('0'),
							_elm_lang$core$Basics$toString(
								_elm_lang$core$Date$year(date)));
					}
				case 'Y':
					var _p7 = length;
					if (_p7 === 2) {
						return A2(
							_elm_lang$core$String$right,
							2,
							A3(
								_elm_lang$core$String$padLeft,
								length,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_justinmimbs$elm_date_extra$Date_Internal_Extract$weekYear(date))));
					} else {
						return A3(
							_elm_lang$core$String$padLeft,
							length,
							_elm_lang$core$Native_Utils.chr('0'),
							_elm_lang$core$Basics$toString(
								_justinmimbs$elm_date_extra$Date_Internal_Extract$weekYear(date)));
					}
				case 'Q':
					var _p8 = length;
					switch (_p8) {
						case 1:
							return _elm_lang$core$Basics$toString(
								_justinmimbs$elm_date_extra$Date_Internal_Extract$quarter(date));
						case 2:
							return _elm_lang$core$Basics$toString(
								_justinmimbs$elm_date_extra$Date_Internal_Extract$quarter(date));
						case 3:
							return A2(
								F2(
									function (x, y) {
										return A2(_elm_lang$core$Basics_ops['++'], x, y);
									}),
								'Q',
								_elm_lang$core$Basics$toString(
									_justinmimbs$elm_date_extra$Date_Internal_Extract$quarter(date)));
						case 4:
							return _justinmimbs$elm_date_extra$Date_Internal_Format$withOrdinalSuffix(
								_justinmimbs$elm_date_extra$Date_Internal_Extract$quarter(date));
						case 5:
							return _elm_lang$core$Basics$toString(
								_justinmimbs$elm_date_extra$Date_Internal_Extract$quarter(date));
						default:
							return '';
					}
				case 'M':
					var _p9 = length;
					switch (_p9) {
						case 1:
							return _elm_lang$core$Basics$toString(
								_justinmimbs$elm_date_extra$Date_Internal_Extract$monthNumber(date));
						case 2:
							return A3(
								_elm_lang$core$String$padLeft,
								2,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_justinmimbs$elm_date_extra$Date_Internal_Extract$monthNumber(date)));
						case 3:
							return A2(
								_elm_lang$core$String$left,
								3,
								_justinmimbs$elm_date_extra$Date_Internal_Format$monthName(
									_elm_lang$core$Date$month(date)));
						case 4:
							return _justinmimbs$elm_date_extra$Date_Internal_Format$monthName(
								_elm_lang$core$Date$month(date));
						case 5:
							return A2(
								_elm_lang$core$String$left,
								1,
								_justinmimbs$elm_date_extra$Date_Internal_Format$monthName(
									_elm_lang$core$Date$month(date)));
						default:
							return '';
					}
				case 'w':
					var _p10 = length;
					switch (_p10) {
						case 1:
							return _elm_lang$core$Basics$toString(
								_justinmimbs$elm_date_extra$Date_Internal_Extract$weekNumber(date));
						case 2:
							return A3(
								_elm_lang$core$String$padLeft,
								2,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_justinmimbs$elm_date_extra$Date_Internal_Extract$weekNumber(date)));
						default:
							return '';
					}
				case 'd':
					var _p11 = length;
					switch (_p11) {
						case 1:
							return _elm_lang$core$Basics$toString(
								_elm_lang$core$Date$day(date));
						case 2:
							return A3(
								_elm_lang$core$String$padLeft,
								2,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_elm_lang$core$Date$day(date)));
						case 3:
							return _justinmimbs$elm_date_extra$Date_Internal_Format$withOrdinalSuffix(
								_elm_lang$core$Date$day(date));
						default:
							return '';
					}
				case 'D':
					var _p12 = length;
					switch (_p12) {
						case 1:
							return _elm_lang$core$Basics$toString(
								_justinmimbs$elm_date_extra$Date_Internal_Extract$ordinalDay(date));
						case 2:
							return A3(
								_elm_lang$core$String$padLeft,
								2,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_justinmimbs$elm_date_extra$Date_Internal_Extract$ordinalDay(date)));
						case 3:
							return A3(
								_elm_lang$core$String$padLeft,
								3,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_justinmimbs$elm_date_extra$Date_Internal_Extract$ordinalDay(date)));
						default:
							return '';
					}
				case 'E':
					var _p13 = _justinmimbs$elm_date_extra$Date_Internal_Format$nameForm(length);
					switch (_p13) {
						case 'abbreviated':
							return A2(
								_elm_lang$core$String$left,
								3,
								_justinmimbs$elm_date_extra$Date_Internal_Format$dayOfWeekName(
									_elm_lang$core$Date$dayOfWeek(date)));
						case 'full':
							return _justinmimbs$elm_date_extra$Date_Internal_Format$dayOfWeekName(
								_elm_lang$core$Date$dayOfWeek(date));
						case 'narrow':
							return A2(
								_elm_lang$core$String$left,
								1,
								_justinmimbs$elm_date_extra$Date_Internal_Format$dayOfWeekName(
									_elm_lang$core$Date$dayOfWeek(date)));
						case 'short':
							return A2(
								_elm_lang$core$String$left,
								2,
								_justinmimbs$elm_date_extra$Date_Internal_Format$dayOfWeekName(
									_elm_lang$core$Date$dayOfWeek(date)));
						default:
							return '';
					}
				case 'e':
					var _p14 = length;
					switch (_p14) {
						case 1:
							return _elm_lang$core$Basics$toString(
								_justinmimbs$elm_date_extra$Date_Internal_Extract$weekdayNumber(date));
						case 2:
							return _elm_lang$core$Basics$toString(
								_justinmimbs$elm_date_extra$Date_Internal_Extract$weekdayNumber(date));
						default:
							var _v15 = asUtc,
								_v16 = date,
								_v17 = _elm_lang$core$String$toUpper(match);
							asUtc = _v15;
							date = _v16;
							match = _v17;
							continue format;
					}
				case 'a':
					var p = _justinmimbs$elm_date_extra$Date_Internal_Format$dayPeriod(date);
					var m = (_elm_lang$core$Native_Utils.eq(p, _justinmimbs$elm_date_extra$Date_Internal_Format$Midnight) || _elm_lang$core$Native_Utils.eq(p, _justinmimbs$elm_date_extra$Date_Internal_Format$AM)) ? 'A' : 'P';
					var _p15 = _justinmimbs$elm_date_extra$Date_Internal_Format$nameForm(length);
					switch (_p15) {
						case 'abbreviated':
							return A2(_elm_lang$core$Basics_ops['++'], m, 'M');
						case 'full':
							return A2(_elm_lang$core$Basics_ops['++'], m, '.M.');
						case 'narrow':
							return m;
						default:
							return '';
					}
				case 'b':
					var _p16 = _justinmimbs$elm_date_extra$Date_Internal_Format$nameForm(length);
					switch (_p16) {
						case 'abbreviated':
							var _p17 = _justinmimbs$elm_date_extra$Date_Internal_Format$dayPeriod(date);
							switch (_p17.ctor) {
								case 'Midnight':
									return 'mid.';
								case 'AM':
									return 'am';
								case 'Noon':
									return 'noon';
								default:
									return 'pm';
							}
						case 'full':
							var _p18 = _justinmimbs$elm_date_extra$Date_Internal_Format$dayPeriod(date);
							switch (_p18.ctor) {
								case 'Midnight':
									return 'midnight';
								case 'AM':
									return 'a.m.';
								case 'Noon':
									return 'noon';
								default:
									return 'p.m.';
							}
						case 'narrow':
							var _p19 = _justinmimbs$elm_date_extra$Date_Internal_Format$dayPeriod(date);
							switch (_p19.ctor) {
								case 'Midnight':
									return 'md';
								case 'AM':
									return 'a';
								case 'Noon':
									return 'nn';
								default:
									return 'p';
							}
						default:
							return '';
					}
				case 'h':
					var _p20 = length;
					switch (_p20) {
						case 1:
							return _elm_lang$core$Basics$toString(
								_justinmimbs$elm_date_extra$Date_Internal_Format$hour12(date));
						case 2:
							return A3(
								_elm_lang$core$String$padLeft,
								2,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_justinmimbs$elm_date_extra$Date_Internal_Format$hour12(date)));
						default:
							return '';
					}
				case 'H':
					var _p21 = length;
					switch (_p21) {
						case 1:
							return _elm_lang$core$Basics$toString(
								_elm_lang$core$Date$hour(date));
						case 2:
							return A3(
								_elm_lang$core$String$padLeft,
								2,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_elm_lang$core$Date$hour(date)));
						default:
							return '';
					}
				case 'm':
					var _p22 = length;
					switch (_p22) {
						case 1:
							return _elm_lang$core$Basics$toString(
								_elm_lang$core$Date$minute(date));
						case 2:
							return A3(
								_elm_lang$core$String$padLeft,
								2,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_elm_lang$core$Date$minute(date)));
						default:
							return '';
					}
				case 's':
					var _p23 = length;
					switch (_p23) {
						case 1:
							return _elm_lang$core$Basics$toString(
								_elm_lang$core$Date$second(date));
						case 2:
							return A3(
								_elm_lang$core$String$padLeft,
								2,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_elm_lang$core$Date$second(date)));
						default:
							return '';
					}
				case 'S':
					return A3(
						_elm_lang$core$String$padRight,
						length,
						_elm_lang$core$Native_Utils.chr('0'),
						A2(
							_elm_lang$core$String$left,
							length,
							A3(
								_elm_lang$core$String$padLeft,
								3,
								_elm_lang$core$Native_Utils.chr('0'),
								_elm_lang$core$Basics$toString(
									_elm_lang$core$Date$millisecond(date)))));
				case 'X':
					if ((_elm_lang$core$Native_Utils.cmp(length, 4) < 0) && (asUtc || _elm_lang$core$Native_Utils.eq(
						_justinmimbs$elm_date_extra$Date_Internal_Extract$offsetFromUtc(date),
						0))) {
						return 'Z';
					} else {
						var _v27 = asUtc,
							_v28 = date,
							_v29 = _elm_lang$core$String$toLower(match);
						asUtc = _v27;
						date = _v28;
						match = _v29;
						continue format;
					}
				case 'x':
					var offset = asUtc ? 0 : _justinmimbs$elm_date_extra$Date_Internal_Extract$offsetFromUtc(date);
					var _p24 = length;
					switch (_p24) {
						case 1:
							return A3(_justinmimbs$elm_date_extra$Date_Internal_Format$formatTimeOffset, '', true, offset);
						case 2:
							return A3(_justinmimbs$elm_date_extra$Date_Internal_Format$formatTimeOffset, '', false, offset);
						case 3:
							return A3(_justinmimbs$elm_date_extra$Date_Internal_Format$formatTimeOffset, ':', false, offset);
						default:
							return '';
					}
				case '\'':
					return _elm_lang$core$Native_Utils.eq(match, '\'\'') ? '\'' : A4(
						_elm_lang$core$Regex$replace,
						_elm_lang$core$Regex$All,
						_elm_lang$core$Regex$regex('\'\''),
						function (_p25) {
							return '\'';
						},
						A3(_elm_lang$core$String$slice, 1, -1, match));
				default:
					return '';
			}
		}
	});
var _justinmimbs$elm_date_extra$Date_Internal_Format$toFormattedString = F3(
	function (asUtc, pattern, date) {
		var date_ = asUtc ? _justinmimbs$elm_date_extra$Date_Internal_Format$toUtc(date) : date;
		return A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_justinmimbs$elm_date_extra$Date_Internal_Format$patternMatches,
			function (_p26) {
				return A3(
					_justinmimbs$elm_date_extra$Date_Internal_Format$format,
					asUtc,
					date_,
					function (_) {
						return _.match;
					}(_p26));
			},
			pattern);
	});

var _justinmimbs$elm_date_extra$Date_Internal_Parse$isoDateRegex = function () {
	var time = 'T(\\d{2})(?:(\\:)?(\\d{2})(?:\\10(\\d{2}))?)?(\\.\\d+)?(?:(Z)|(?:([+\\-])(\\d{2})(?:\\:?(\\d{2}))?))?';
	var ord = '\\-?(\\d{3})';
	var week = '(\\-)?W(\\d{2})(?:\\5(\\d))?';
	var cal = '(\\-)?(\\d{2})(?:\\2(\\d{2}))?';
	var year = '(\\d{4})';
	return _elm_lang$core$Regex$regex(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'^',
			A2(
				_elm_lang$core$Basics_ops['++'],
				year,
				A2(
					_elm_lang$core$Basics_ops['++'],
					'(?:',
					A2(
						_elm_lang$core$Basics_ops['++'],
						cal,
						A2(
							_elm_lang$core$Basics_ops['++'],
							'|',
							A2(
								_elm_lang$core$Basics_ops['++'],
								week,
								A2(
									_elm_lang$core$Basics_ops['++'],
									'|',
									A2(
										_elm_lang$core$Basics_ops['++'],
										ord,
										A2(
											_elm_lang$core$Basics_ops['++'],
											')?',
											A2(
												_elm_lang$core$Basics_ops['++'],
												'(?:',
												A2(_elm_lang$core$Basics_ops['++'], time, ')?$'))))))))))));
}();
var _justinmimbs$elm_date_extra$Date_Internal_Parse$stringToFloat = function (_p0) {
	return _elm_lang$core$Result$toMaybe(
		_elm_lang$core$String$toFloat(_p0));
};
var _justinmimbs$elm_date_extra$Date_Internal_Parse$msFromMatches = F4(
	function (timeHH, timeMM, timeSS, timeF) {
		var fractional = A2(
			_elm_lang$core$Maybe$withDefault,
			0.0,
			A2(_elm_lang$core$Maybe$andThen, _justinmimbs$elm_date_extra$Date_Internal_Parse$stringToFloat, timeF));
		var _p1 = function () {
			var _p2 = A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Maybe$andThen(_justinmimbs$elm_date_extra$Date_Internal_Parse$stringToFloat),
				{
					ctor: '::',
					_0: timeHH,
					_1: {
						ctor: '::',
						_0: timeMM,
						_1: {
							ctor: '::',
							_0: timeSS,
							_1: {ctor: '[]'}
						}
					}
				});
			_v0_3:
			do {
				if (((_p2.ctor === '::') && (_p2._0.ctor === 'Just')) && (_p2._1.ctor === '::')) {
					if (_p2._1._0.ctor === 'Just') {
						if (_p2._1._1.ctor === '::') {
							if (_p2._1._1._0.ctor === 'Just') {
								if (_p2._1._1._1.ctor === '[]') {
									return {ctor: '_Tuple3', _0: _p2._0._0, _1: _p2._1._0._0, _2: _p2._1._1._0._0 + fractional};
								} else {
									break _v0_3;
								}
							} else {
								if (_p2._1._1._1.ctor === '[]') {
									return {ctor: '_Tuple3', _0: _p2._0._0, _1: _p2._1._0._0 + fractional, _2: 0.0};
								} else {
									break _v0_3;
								}
							}
						} else {
							break _v0_3;
						}
					} else {
						if (((_p2._1._1.ctor === '::') && (_p2._1._1._0.ctor === 'Nothing')) && (_p2._1._1._1.ctor === '[]')) {
							return {ctor: '_Tuple3', _0: _p2._0._0 + fractional, _1: 0.0, _2: 0.0};
						} else {
							break _v0_3;
						}
					}
				} else {
					break _v0_3;
				}
			} while(false);
			return {ctor: '_Tuple3', _0: 0.0, _1: 0.0, _2: 0.0};
		}();
		var hh = _p1._0;
		var mm = _p1._1;
		var ss = _p1._2;
		return _elm_lang$core$Basics$round(
			((hh * _elm_lang$core$Basics$toFloat(_justinmimbs$elm_date_extra$Date_Extra_Facts$msPerHour)) + (mm * _elm_lang$core$Basics$toFloat(_justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute))) + (ss * _elm_lang$core$Basics$toFloat(_justinmimbs$elm_date_extra$Date_Extra_Facts$msPerSecond)));
	});
var _justinmimbs$elm_date_extra$Date_Internal_Parse$stringToInt = function (_p3) {
	return _elm_lang$core$Result$toMaybe(
		_elm_lang$core$String$toInt(_p3));
};
var _justinmimbs$elm_date_extra$Date_Internal_Parse$unixTimeFromMatches = F6(
	function (yyyy, calMM, calDD, weekWW, weekD, ordDDD) {
		var y = A2(
			_elm_lang$core$Maybe$withDefault,
			1,
			_justinmimbs$elm_date_extra$Date_Internal_Parse$stringToInt(yyyy));
		var _p4 = {ctor: '_Tuple2', _0: calMM, _1: weekWW};
		_v1_2:
		do {
			if (_p4.ctor === '_Tuple2') {
				if (_p4._0.ctor === 'Just') {
					if (_p4._1.ctor === 'Nothing') {
						return A3(
							_justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromCalendarDate,
							y,
							_justinmimbs$elm_date_extra$Date_Extra_Facts$monthFromMonthNumber(
								A2(
									_elm_lang$core$Maybe$withDefault,
									1,
									A2(_elm_lang$core$Maybe$andThen, _justinmimbs$elm_date_extra$Date_Internal_Parse$stringToInt, calMM))),
							A2(
								_elm_lang$core$Maybe$withDefault,
								1,
								A2(_elm_lang$core$Maybe$andThen, _justinmimbs$elm_date_extra$Date_Internal_Parse$stringToInt, calDD)));
					} else {
						break _v1_2;
					}
				} else {
					if (_p4._1.ctor === 'Just') {
						return A3(
							_justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromWeekDate,
							y,
							A2(
								_elm_lang$core$Maybe$withDefault,
								1,
								A2(_elm_lang$core$Maybe$andThen, _justinmimbs$elm_date_extra$Date_Internal_Parse$stringToInt, weekWW)),
							A2(
								_elm_lang$core$Maybe$withDefault,
								1,
								A2(_elm_lang$core$Maybe$andThen, _justinmimbs$elm_date_extra$Date_Internal_Parse$stringToInt, weekD)));
					} else {
						break _v1_2;
					}
				}
			} else {
				break _v1_2;
			}
		} while(false);
		return A2(
			_justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromOrdinalDate,
			y,
			A2(
				_elm_lang$core$Maybe$withDefault,
				1,
				A2(_elm_lang$core$Maybe$andThen, _justinmimbs$elm_date_extra$Date_Internal_Parse$stringToInt, ordDDD)));
	});
var _justinmimbs$elm_date_extra$Date_Internal_Parse$offsetFromMatches = F4(
	function (tzZ, tzSign, tzHH, tzMM) {
		var _p5 = {ctor: '_Tuple2', _0: tzZ, _1: tzSign};
		_v2_2:
		do {
			if (_p5.ctor === '_Tuple2') {
				if (_p5._0.ctor === 'Just') {
					if ((_p5._0._0 === 'Z') && (_p5._1.ctor === 'Nothing')) {
						return _elm_lang$core$Maybe$Just(0);
					} else {
						break _v2_2;
					}
				} else {
					if (_p5._1.ctor === 'Just') {
						var mm = A2(
							_elm_lang$core$Maybe$withDefault,
							0,
							A2(_elm_lang$core$Maybe$andThen, _justinmimbs$elm_date_extra$Date_Internal_Parse$stringToInt, tzMM));
						var hh = A2(
							_elm_lang$core$Maybe$withDefault,
							0,
							A2(_elm_lang$core$Maybe$andThen, _justinmimbs$elm_date_extra$Date_Internal_Parse$stringToInt, tzHH));
						return _elm_lang$core$Maybe$Just(
							(_elm_lang$core$Native_Utils.eq(_p5._1._0, '+') ? 1 : -1) * ((hh * 60) + mm));
					} else {
						break _v2_2;
					}
				}
			} else {
				break _v2_2;
			}
		} while(false);
		return _elm_lang$core$Maybe$Nothing;
	});
var _justinmimbs$elm_date_extra$Date_Internal_Parse$offsetTimeFromMatches = function (matches) {
	var _p6 = matches;
	if (((((((((((((((((((_p6.ctor === '::') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === '::')) && (_p6._1._1.ctor === '::')) && (_p6._1._1._1.ctor === '::')) && (_p6._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1._1._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.ctor === '::')) && (_p6._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.ctor === '[]')) {
		var offset = A4(_justinmimbs$elm_date_extra$Date_Internal_Parse$offsetFromMatches, _p6._1._1._1._1._1._1._1._1._1._1._1._1._1._0, _p6._1._1._1._1._1._1._1._1._1._1._1._1._1._1._0, _p6._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._0, _p6._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._0);
		var timeMS = A4(_justinmimbs$elm_date_extra$Date_Internal_Parse$msFromMatches, _p6._1._1._1._1._1._1._1._1._0, _p6._1._1._1._1._1._1._1._1._1._1._0, _p6._1._1._1._1._1._1._1._1._1._1._1._0, _p6._1._1._1._1._1._1._1._1._1._1._1._1._0);
		var dateMS = A6(_justinmimbs$elm_date_extra$Date_Internal_Parse$unixTimeFromMatches, _p6._0._0, _p6._1._1._0, _p6._1._1._1._0, _p6._1._1._1._1._1._0, _p6._1._1._1._1._1._1._0, _p6._1._1._1._1._1._1._1._0);
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: offset, _1: dateMS + timeMS});
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _justinmimbs$elm_date_extra$Date_Internal_Parse$offsetTimeFromIsoString = function (s) {
	return A2(
		_elm_lang$core$Maybe$andThen,
		_justinmimbs$elm_date_extra$Date_Internal_Parse$offsetTimeFromMatches,
		A2(
			_elm_lang$core$Maybe$map,
			function (_) {
				return _.submatches;
			},
			_elm_lang$core$List$head(
				A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_justinmimbs$elm_date_extra$Date_Internal_Parse$isoDateRegex,
					s))));
};

var _justinmimbs$elm_date_extra$Date_Extra$toParts = function (date) {
	return {
		ctor: '_Tuple7',
		_0: _elm_lang$core$Date$year(date),
		_1: _elm_lang$core$Date$month(date),
		_2: _elm_lang$core$Date$day(date),
		_3: _elm_lang$core$Date$hour(date),
		_4: _elm_lang$core$Date$minute(date),
		_5: _elm_lang$core$Date$second(date),
		_6: _elm_lang$core$Date$millisecond(date)
	};
};
var _justinmimbs$elm_date_extra$Date_Extra$monthFromQuarter = function (q) {
	var _p0 = q;
	switch (_p0) {
		case 1:
			return _elm_lang$core$Date$Jan;
		case 2:
			return _elm_lang$core$Date$Apr;
		case 3:
			return _elm_lang$core$Date$Jul;
		default:
			return _elm_lang$core$Date$Oct;
	}
};
var _justinmimbs$elm_date_extra$Date_Extra$clamp = F3(
	function (min, max, date) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$Date$toTime(date),
			_elm_lang$core$Date$toTime(min)) < 0) ? min : ((_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$Date$toTime(date),
			_elm_lang$core$Date$toTime(max)) > 0) ? max : date);
	});
var _justinmimbs$elm_date_extra$Date_Extra$comparableIsBetween = F3(
	function (a, b, x) {
		return ((_elm_lang$core$Native_Utils.cmp(a, x) < 1) && (_elm_lang$core$Native_Utils.cmp(x, b) < 1)) || ((_elm_lang$core$Native_Utils.cmp(b, x) < 1) && (_elm_lang$core$Native_Utils.cmp(x, a) < 1));
	});
var _justinmimbs$elm_date_extra$Date_Extra$isBetween = F3(
	function (date1, date2, date) {
		return A3(
			_justinmimbs$elm_date_extra$Date_Extra$comparableIsBetween,
			_elm_lang$core$Date$toTime(date1),
			_elm_lang$core$Date$toTime(date2),
			_elm_lang$core$Date$toTime(date));
	});
var _justinmimbs$elm_date_extra$Date_Extra$compare = F2(
	function (a, b) {
		return A2(
			_elm_lang$core$Basics$compare,
			_elm_lang$core$Date$toTime(a),
			_elm_lang$core$Date$toTime(b));
	});
var _justinmimbs$elm_date_extra$Date_Extra$equal = F2(
	function (a, b) {
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$Date$toTime(a),
			_elm_lang$core$Date$toTime(b));
	});
var _justinmimbs$elm_date_extra$Date_Extra$offsetFromUtc = _justinmimbs$elm_date_extra$Date_Internal_Extract$offsetFromUtc;
var _justinmimbs$elm_date_extra$Date_Extra$weekYear = _justinmimbs$elm_date_extra$Date_Internal_Extract$weekYear;
var _justinmimbs$elm_date_extra$Date_Extra$weekNumber = _justinmimbs$elm_date_extra$Date_Internal_Extract$weekNumber;
var _justinmimbs$elm_date_extra$Date_Extra$weekdayNumber = _justinmimbs$elm_date_extra$Date_Internal_Extract$weekdayNumber;
var _justinmimbs$elm_date_extra$Date_Extra$daysToPreviousDayOfWeek = F2(
	function (d, date) {
		return _elm_lang$core$Basics$negate(
			A2(
				_elm_lang$core$Basics_ops['%'],
				(_justinmimbs$elm_date_extra$Date_Extra$weekdayNumber(date) - _justinmimbs$elm_date_extra$Date_Extra_Facts$weekdayNumberFromDayOfWeek(d)) + 7,
				7));
	});
var _justinmimbs$elm_date_extra$Date_Extra$fractionalDay = _justinmimbs$elm_date_extra$Date_Internal_Extract$fractionalDay;
var _justinmimbs$elm_date_extra$Date_Extra$ordinalDay = _justinmimbs$elm_date_extra$Date_Internal_Extract$ordinalDay;
var _justinmimbs$elm_date_extra$Date_Extra$quarter = _justinmimbs$elm_date_extra$Date_Internal_Extract$quarter;
var _justinmimbs$elm_date_extra$Date_Extra$monthNumber = _justinmimbs$elm_date_extra$Date_Internal_Extract$monthNumber;
var _justinmimbs$elm_date_extra$Date_Extra$ordinalMonth = function (date) {
	return (_elm_lang$core$Date$year(date) * 12) + _justinmimbs$elm_date_extra$Date_Extra$monthNumber(date);
};
var _justinmimbs$elm_date_extra$Date_Extra$diffMonth = F2(
	function (date1, date2) {
		var fractionalMonth = function (date) {
			return (_elm_lang$core$Basics$toFloat(
				_elm_lang$core$Date$day(date) - 1) + _justinmimbs$elm_date_extra$Date_Extra$fractionalDay(date)) / 31;
		};
		var ordinalMonthFloat = function (date) {
			return _elm_lang$core$Basics$toFloat(
				_justinmimbs$elm_date_extra$Date_Extra$ordinalMonth(date)) + fractionalMonth(date);
		};
		return _elm_lang$core$Basics$truncate(
			ordinalMonthFloat(date2) - ordinalMonthFloat(date1));
	});
var _justinmimbs$elm_date_extra$Date_Extra$toUtcFormattedString = _justinmimbs$elm_date_extra$Date_Internal_Format$toFormattedString(true);
var _justinmimbs$elm_date_extra$Date_Extra$toUtcIsoString = _justinmimbs$elm_date_extra$Date_Extra$toUtcFormattedString('yyyy-MM-dd\'T\'HH:mm:ss.SSSXXX');
var _justinmimbs$elm_date_extra$Date_Extra$toFormattedString = _justinmimbs$elm_date_extra$Date_Internal_Format$toFormattedString(false);
var _justinmimbs$elm_date_extra$Date_Extra$toIsoString = _justinmimbs$elm_date_extra$Date_Extra$toFormattedString('yyyy-MM-dd\'T\'HH:mm:ss.SSSxxx');
var _justinmimbs$elm_date_extra$Date_Extra$fromTime = function (_p1) {
	return _elm_lang$core$Date$fromTime(
		_elm_lang$core$Basics$toFloat(_p1));
};
var _justinmimbs$elm_date_extra$Date_Extra$fromOffsetTime = function (_p2) {
	var _p3 = _p2;
	var _p5 = _p3._1;
	var _p4 = _p3._0;
	if (_p4.ctor === 'Just') {
		return _justinmimbs$elm_date_extra$Date_Extra$fromTime(_p5 - (_justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute * _p4._0));
	} else {
		var offset0 = _justinmimbs$elm_date_extra$Date_Extra$offsetFromUtc(
			_justinmimbs$elm_date_extra$Date_Extra$fromTime(_p5));
		var date1 = _justinmimbs$elm_date_extra$Date_Extra$fromTime(_p5 - (_justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute * offset0));
		var offset1 = _justinmimbs$elm_date_extra$Date_Extra$offsetFromUtc(date1);
		if (_elm_lang$core$Native_Utils.eq(offset0, offset1)) {
			return date1;
		} else {
			var date2 = _justinmimbs$elm_date_extra$Date_Extra$fromTime(_p5 - (_justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute * offset1));
			var offset2 = _justinmimbs$elm_date_extra$Date_Extra$offsetFromUtc(date2);
			return _elm_lang$core$Native_Utils.eq(offset1, offset2) ? date2 : date1;
		}
	}
};
var _justinmimbs$elm_date_extra$Date_Extra$fromParts = F7(
	function (y, m, d, hh, mm, ss, ms) {
		return _justinmimbs$elm_date_extra$Date_Extra$fromOffsetTime(
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Maybe$Nothing,
				_1: A7(_justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromParts, y, m, d, hh, mm, ss, ms)
			});
	});
var _justinmimbs$elm_date_extra$Date_Extra$addMonths = F2(
	function (n, date) {
		var om = (_justinmimbs$elm_date_extra$Date_Extra$ordinalMonth(date) + n) + -1;
		var y_ = (om / 12) | 0;
		var m_ = _justinmimbs$elm_date_extra$Date_Extra_Facts$monthFromMonthNumber(
			A2(_elm_lang$core$Basics_ops['%'], om, 12) + 1);
		var _p6 = _justinmimbs$elm_date_extra$Date_Extra$toParts(date);
		var y = _p6._0;
		var m = _p6._1;
		var d = _p6._2;
		var hh = _p6._3;
		var mm = _p6._4;
		var ss = _p6._5;
		var ms = _p6._6;
		var d_ = A2(
			_elm_lang$core$Basics$min,
			d,
			A2(_justinmimbs$elm_date_extra$Date_Extra_Facts$daysInMonth, y_, m_));
		return A7(_justinmimbs$elm_date_extra$Date_Extra$fromParts, y_, m_, d_, hh, mm, ss, ms);
	});
var _justinmimbs$elm_date_extra$Date_Extra$add = F3(
	function (interval, n, date) {
		var _p7 = _justinmimbs$elm_date_extra$Date_Extra$toParts(date);
		var y = _p7._0;
		var m = _p7._1;
		var d = _p7._2;
		var hh = _p7._3;
		var mm = _p7._4;
		var ss = _p7._5;
		var ms = _p7._6;
		var _p8 = interval;
		switch (_p8.ctor) {
			case 'Millisecond':
				return _elm_lang$core$Date$fromTime(
					_elm_lang$core$Date$toTime(date) + _elm_lang$core$Basics$toFloat(n));
			case 'Second':
				return _elm_lang$core$Date$fromTime(
					_elm_lang$core$Date$toTime(date) + _elm_lang$core$Basics$toFloat(n * _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerSecond));
			case 'Minute':
				return _elm_lang$core$Date$fromTime(
					_elm_lang$core$Date$toTime(date) + _elm_lang$core$Basics$toFloat(n * _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute));
			case 'Hour':
				return _elm_lang$core$Date$fromTime(
					_elm_lang$core$Date$toTime(date) + _elm_lang$core$Basics$toFloat(n * _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerHour));
			case 'Day':
				return A7(_justinmimbs$elm_date_extra$Date_Extra$fromParts, y, m, d + n, hh, mm, ss, ms);
			case 'Month':
				return A2(_justinmimbs$elm_date_extra$Date_Extra$addMonths, n, date);
			case 'Year':
				return A2(_justinmimbs$elm_date_extra$Date_Extra$addMonths, n * 12, date);
			case 'Quarter':
				return A2(_justinmimbs$elm_date_extra$Date_Extra$addMonths, n * 3, date);
			case 'Week':
				return A7(_justinmimbs$elm_date_extra$Date_Extra$fromParts, y, m, d + (n * 7), hh, mm, ss, ms);
			default:
				return A7(_justinmimbs$elm_date_extra$Date_Extra$fromParts, y, m, d + (n * 7), hh, mm, ss, ms);
		}
	});
var _justinmimbs$elm_date_extra$Date_Extra$rangeHelp = F5(
	function (result, interval, step, start, date) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(
				_elm_lang$core$Date$toTime(date),
				_elm_lang$core$Date$toTime(start)) < 0) {
				return result;
			} else {
				var _v4 = {ctor: '::', _0: date, _1: result},
					_v5 = interval,
					_v6 = step,
					_v7 = start,
					_v8 = A3(_justinmimbs$elm_date_extra$Date_Extra$add, interval, step, date);
				result = _v4;
				interval = _v5;
				step = _v6;
				start = _v7;
				date = _v8;
				continue rangeHelp;
			}
		}
	});
var _justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate = F3(
	function (y, m, d) {
		return _justinmimbs$elm_date_extra$Date_Extra$fromOffsetTime(
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Maybe$Nothing,
				_1: A3(_justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromCalendarDate, y, m, d)
			});
	});
var _justinmimbs$elm_date_extra$Date_Extra$floor = F2(
	function (interval, date) {
		var _p9 = _justinmimbs$elm_date_extra$Date_Extra$toParts(date);
		var y = _p9._0;
		var m = _p9._1;
		var d = _p9._2;
		var hh = _p9._3;
		var mm = _p9._4;
		var ss = _p9._5;
		var _p10 = interval;
		switch (_p10.ctor) {
			case 'Millisecond':
				return date;
			case 'Second':
				return A7(_justinmimbs$elm_date_extra$Date_Extra$fromParts, y, m, d, hh, mm, ss, 0);
			case 'Minute':
				return A7(_justinmimbs$elm_date_extra$Date_Extra$fromParts, y, m, d, hh, mm, 0, 0);
			case 'Hour':
				return A7(_justinmimbs$elm_date_extra$Date_Extra$fromParts, y, m, d, hh, 0, 0, 0);
			case 'Day':
				return A3(_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate, y, m, d);
			case 'Month':
				return A3(_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate, y, m, 1);
			case 'Year':
				return A3(_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate, y, _elm_lang$core$Date$Jan, 1);
			case 'Quarter':
				return A3(
					_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate,
					y,
					_justinmimbs$elm_date_extra$Date_Extra$monthFromQuarter(
						_justinmimbs$elm_date_extra$Date_Extra$quarter(date)),
					1);
			case 'Week':
				return A3(
					_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate,
					y,
					m,
					d + A2(_justinmimbs$elm_date_extra$Date_Extra$daysToPreviousDayOfWeek, _elm_lang$core$Date$Mon, date));
			case 'Monday':
				return A3(
					_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate,
					y,
					m,
					d + A2(_justinmimbs$elm_date_extra$Date_Extra$daysToPreviousDayOfWeek, _elm_lang$core$Date$Mon, date));
			case 'Tuesday':
				return A3(
					_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate,
					y,
					m,
					d + A2(_justinmimbs$elm_date_extra$Date_Extra$daysToPreviousDayOfWeek, _elm_lang$core$Date$Tue, date));
			case 'Wednesday':
				return A3(
					_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate,
					y,
					m,
					d + A2(_justinmimbs$elm_date_extra$Date_Extra$daysToPreviousDayOfWeek, _elm_lang$core$Date$Wed, date));
			case 'Thursday':
				return A3(
					_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate,
					y,
					m,
					d + A2(_justinmimbs$elm_date_extra$Date_Extra$daysToPreviousDayOfWeek, _elm_lang$core$Date$Thu, date));
			case 'Friday':
				return A3(
					_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate,
					y,
					m,
					d + A2(_justinmimbs$elm_date_extra$Date_Extra$daysToPreviousDayOfWeek, _elm_lang$core$Date$Fri, date));
			case 'Saturday':
				return A3(
					_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate,
					y,
					m,
					d + A2(_justinmimbs$elm_date_extra$Date_Extra$daysToPreviousDayOfWeek, _elm_lang$core$Date$Sat, date));
			default:
				return A3(
					_justinmimbs$elm_date_extra$Date_Extra$fromCalendarDate,
					y,
					m,
					d + A2(_justinmimbs$elm_date_extra$Date_Extra$daysToPreviousDayOfWeek, _elm_lang$core$Date$Sun, date));
		}
	});
var _justinmimbs$elm_date_extra$Date_Extra$ceiling = F2(
	function (interval, date) {
		var floored = A2(_justinmimbs$elm_date_extra$Date_Extra$floor, interval, date);
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$Date$toTime(date),
			_elm_lang$core$Date$toTime(floored)) ? date : A3(_justinmimbs$elm_date_extra$Date_Extra$add, interval, 1, floored);
	});
var _justinmimbs$elm_date_extra$Date_Extra$range = F4(
	function (interval, step, start, end) {
		var stepBack = _elm_lang$core$Basics$negate(
			A2(_elm_lang$core$Basics$max, 1, step));
		return A5(
			_justinmimbs$elm_date_extra$Date_Extra$rangeHelp,
			{ctor: '[]'},
			interval,
			stepBack,
			start,
			A2(
				_justinmimbs$elm_date_extra$Date_Extra$ceiling,
				interval,
				A3(_justinmimbs$elm_date_extra$Date_Extra$add, interval, stepBack, end)));
	});
var _justinmimbs$elm_date_extra$Date_Extra$fromIsoString = function (_p11) {
	return A2(
		_elm_lang$core$Maybe$map,
		_justinmimbs$elm_date_extra$Date_Extra$fromOffsetTime,
		_justinmimbs$elm_date_extra$Date_Internal_Parse$offsetTimeFromIsoString(_p11));
};
var _justinmimbs$elm_date_extra$Date_Extra$fromSpec = F3(
	function (_p14, _p13, _p12) {
		var _p15 = _p14;
		var _p16 = _p13;
		var _p17 = _p12;
		return _justinmimbs$elm_date_extra$Date_Extra$fromOffsetTime(
			{ctor: '_Tuple2', _0: _p15._0, _1: _p17._0 + _p16._0});
	});
var _justinmimbs$elm_date_extra$Date_Extra$Offset = function (a) {
	return {ctor: 'Offset', _0: a};
};
var _justinmimbs$elm_date_extra$Date_Extra$utc = _justinmimbs$elm_date_extra$Date_Extra$Offset(
	_elm_lang$core$Maybe$Just(0));
var _justinmimbs$elm_date_extra$Date_Extra$offset = function (minutes) {
	return _justinmimbs$elm_date_extra$Date_Extra$Offset(
		_elm_lang$core$Maybe$Just(minutes));
};
var _justinmimbs$elm_date_extra$Date_Extra$local = _justinmimbs$elm_date_extra$Date_Extra$Offset(_elm_lang$core$Maybe$Nothing);
var _justinmimbs$elm_date_extra$Date_Extra$TimeMS = function (a) {
	return {ctor: 'TimeMS', _0: a};
};
var _justinmimbs$elm_date_extra$Date_Extra$noTime = _justinmimbs$elm_date_extra$Date_Extra$TimeMS(0);
var _justinmimbs$elm_date_extra$Date_Extra$atTime = F4(
	function (hh, mm, ss, ms) {
		return _justinmimbs$elm_date_extra$Date_Extra$TimeMS(
			A4(_justinmimbs$elm_date_extra$Date_Internal_Core$msFromTimeParts, hh, mm, ss, ms));
	});
var _justinmimbs$elm_date_extra$Date_Extra$DateMS = function (a) {
	return {ctor: 'DateMS', _0: a};
};
var _justinmimbs$elm_date_extra$Date_Extra$calendarDate = F3(
	function (y, m, d) {
		return _justinmimbs$elm_date_extra$Date_Extra$DateMS(
			A3(_justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromCalendarDate, y, m, d));
	});
var _justinmimbs$elm_date_extra$Date_Extra$ordinalDate = F2(
	function (y, d) {
		return _justinmimbs$elm_date_extra$Date_Extra$DateMS(
			A2(_justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromOrdinalDate, y, d));
	});
var _justinmimbs$elm_date_extra$Date_Extra$weekDate = F3(
	function (y, w, d) {
		return _justinmimbs$elm_date_extra$Date_Extra$DateMS(
			A3(_justinmimbs$elm_date_extra$Date_Internal_Core$unixTimeFromWeekDate, y, w, d));
	});
var _justinmimbs$elm_date_extra$Date_Extra$Sunday = {ctor: 'Sunday'};
var _justinmimbs$elm_date_extra$Date_Extra$Saturday = {ctor: 'Saturday'};
var _justinmimbs$elm_date_extra$Date_Extra$Friday = {ctor: 'Friday'};
var _justinmimbs$elm_date_extra$Date_Extra$Thursday = {ctor: 'Thursday'};
var _justinmimbs$elm_date_extra$Date_Extra$Wednesday = {ctor: 'Wednesday'};
var _justinmimbs$elm_date_extra$Date_Extra$Tuesday = {ctor: 'Tuesday'};
var _justinmimbs$elm_date_extra$Date_Extra$Monday = {ctor: 'Monday'};
var _justinmimbs$elm_date_extra$Date_Extra$Week = {ctor: 'Week'};
var _justinmimbs$elm_date_extra$Date_Extra$Quarter = {ctor: 'Quarter'};
var _justinmimbs$elm_date_extra$Date_Extra$Year = {ctor: 'Year'};
var _justinmimbs$elm_date_extra$Date_Extra$Month = {ctor: 'Month'};
var _justinmimbs$elm_date_extra$Date_Extra$Day = {ctor: 'Day'};
var _justinmimbs$elm_date_extra$Date_Extra$diff = F3(
	function (interval, date1, date2) {
		var diffMS = _elm_lang$core$Basics$floor(
			_elm_lang$core$Date$toTime(date2) - _elm_lang$core$Date$toTime(date1));
		var _p18 = interval;
		switch (_p18.ctor) {
			case 'Millisecond':
				return diffMS;
			case 'Second':
				return (diffMS / _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerSecond) | 0;
			case 'Minute':
				return (diffMS / _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerMinute) | 0;
			case 'Hour':
				return (diffMS / _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerHour) | 0;
			case 'Day':
				return (diffMS / _justinmimbs$elm_date_extra$Date_Extra_Facts$msPerDay) | 0;
			case 'Month':
				return A2(_justinmimbs$elm_date_extra$Date_Extra$diffMonth, date1, date2);
			case 'Year':
				return (A2(_justinmimbs$elm_date_extra$Date_Extra$diffMonth, date1, date2) / 12) | 0;
			case 'Quarter':
				return (A2(_justinmimbs$elm_date_extra$Date_Extra$diffMonth, date1, date2) / 3) | 0;
			case 'Week':
				return (A3(_justinmimbs$elm_date_extra$Date_Extra$diff, _justinmimbs$elm_date_extra$Date_Extra$Day, date1, date2) / 7) | 0;
			default:
				var _p19 = _p18;
				return (A3(
					_justinmimbs$elm_date_extra$Date_Extra$diff,
					_justinmimbs$elm_date_extra$Date_Extra$Day,
					A2(_justinmimbs$elm_date_extra$Date_Extra$floor, _p19, date1),
					A2(_justinmimbs$elm_date_extra$Date_Extra$floor, _p19, date2)) / 7) | 0;
		}
	});
var _justinmimbs$elm_date_extra$Date_Extra$Hour = {ctor: 'Hour'};
var _justinmimbs$elm_date_extra$Date_Extra$Minute = {ctor: 'Minute'};
var _justinmimbs$elm_date_extra$Date_Extra$equalBy = F3(
	function (interval, date1, date2) {
		equalBy:
		while (true) {
			var _p20 = interval;
			switch (_p20.ctor) {
				case 'Millisecond':
					return _elm_lang$core$Native_Utils.eq(
						_elm_lang$core$Date$toTime(date1),
						_elm_lang$core$Date$toTime(date2));
				case 'Second':
					return _elm_lang$core$Native_Utils.eq(
						_elm_lang$core$Date$second(date1),
						_elm_lang$core$Date$second(date2)) && A3(_justinmimbs$elm_date_extra$Date_Extra$equalBy, _justinmimbs$elm_date_extra$Date_Extra$Minute, date1, date2);
				case 'Minute':
					return _elm_lang$core$Native_Utils.eq(
						_elm_lang$core$Date$minute(date1),
						_elm_lang$core$Date$minute(date2)) && A3(_justinmimbs$elm_date_extra$Date_Extra$equalBy, _justinmimbs$elm_date_extra$Date_Extra$Hour, date1, date2);
				case 'Hour':
					return _elm_lang$core$Native_Utils.eq(
						_elm_lang$core$Date$hour(date1),
						_elm_lang$core$Date$hour(date2)) && A3(_justinmimbs$elm_date_extra$Date_Extra$equalBy, _justinmimbs$elm_date_extra$Date_Extra$Day, date1, date2);
				case 'Day':
					return _elm_lang$core$Native_Utils.eq(
						_elm_lang$core$Date$day(date1),
						_elm_lang$core$Date$day(date2)) && A3(_justinmimbs$elm_date_extra$Date_Extra$equalBy, _justinmimbs$elm_date_extra$Date_Extra$Month, date1, date2);
				case 'Month':
					return _elm_lang$core$Native_Utils.eq(
						_elm_lang$core$Date$month(date1),
						_elm_lang$core$Date$month(date2)) && A3(_justinmimbs$elm_date_extra$Date_Extra$equalBy, _justinmimbs$elm_date_extra$Date_Extra$Year, date1, date2);
				case 'Year':
					return _elm_lang$core$Native_Utils.eq(
						_elm_lang$core$Date$year(date1),
						_elm_lang$core$Date$year(date2));
				case 'Quarter':
					return _elm_lang$core$Native_Utils.eq(
						_justinmimbs$elm_date_extra$Date_Extra$quarter(date1),
						_justinmimbs$elm_date_extra$Date_Extra$quarter(date2)) && A3(_justinmimbs$elm_date_extra$Date_Extra$equalBy, _justinmimbs$elm_date_extra$Date_Extra$Year, date1, date2);
				case 'Week':
					return _elm_lang$core$Native_Utils.eq(
						_justinmimbs$elm_date_extra$Date_Extra$weekNumber(date1),
						_justinmimbs$elm_date_extra$Date_Extra$weekNumber(date2)) && _elm_lang$core$Native_Utils.eq(
						_justinmimbs$elm_date_extra$Date_Extra$weekYear(date1),
						_justinmimbs$elm_date_extra$Date_Extra$weekYear(date2));
				default:
					var _p21 = _p20;
					var _v15 = _justinmimbs$elm_date_extra$Date_Extra$Day,
						_v16 = A2(_justinmimbs$elm_date_extra$Date_Extra$floor, _p21, date1),
						_v17 = A2(_justinmimbs$elm_date_extra$Date_Extra$floor, _p21, date2);
					interval = _v15;
					date1 = _v16;
					date2 = _v17;
					continue equalBy;
			}
		}
	});
var _justinmimbs$elm_date_extra$Date_Extra$Second = {ctor: 'Second'};
var _justinmimbs$elm_date_extra$Date_Extra$Millisecond = {ctor: 'Millisecond'};

var _alpacaaa$elm_date_distance$Date_Distance_I18n_En$formatInterval = function (_p0) {
	return _elm_lang$core$String$toLower(
		_elm_lang$core$Basics$toString(_p0));
};
var _alpacaaa$elm_date_distance$Date_Distance_I18n_En$singular = function (interval) {
	var _p1 = interval;
	if (_p1.ctor === 'Minute') {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'a ',
			_alpacaaa$elm_date_distance$Date_Distance_I18n_En$formatInterval(interval));
	} else {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'1 ',
			_alpacaaa$elm_date_distance$Date_Distance_I18n_En$formatInterval(interval));
	}
};
var _alpacaaa$elm_date_distance$Date_Distance_I18n_En$circa = F3(
	function (prefix, interval, i) {
		var _p2 = i;
		if (_p2 === 1) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				prefix,
				A2(
					_elm_lang$core$Basics_ops['++'],
					' ',
					_alpacaaa$elm_date_distance$Date_Distance_I18n_En$singular(interval)));
		} else {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				prefix,
				A2(
					_elm_lang$core$Basics_ops['++'],
					' ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(i),
						A2(
							_elm_lang$core$Basics_ops['++'],
							' ',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_alpacaaa$elm_date_distance$Date_Distance_I18n_En$formatInterval(interval),
								's')))));
		}
	});
var _alpacaaa$elm_date_distance$Date_Distance_I18n_En$exact = F2(
	function (interval, i) {
		var _p3 = i;
		if (_p3 === 1) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'1 ',
				_alpacaaa$elm_date_distance$Date_Distance_I18n_En$formatInterval(interval));
		} else {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(i),
				A2(
					_elm_lang$core$Basics_ops['++'],
					' ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_alpacaaa$elm_date_distance$Date_Distance_I18n_En$formatInterval(interval),
						's')));
		}
	});
var _alpacaaa$elm_date_distance$Date_Distance_I18n_En$locale_ = function (distance) {
	var _p4 = distance;
	switch (_p4.ctor) {
		case 'LessThanXSeconds':
			return A3(_alpacaaa$elm_date_distance$Date_Distance_I18n_En$circa, 'less than', _justinmimbs$elm_date_extra$Date_Extra$Second, _p4._0);
		case 'HalfAMinute':
			return 'half a minute';
		case 'LessThanXMinutes':
			return A3(_alpacaaa$elm_date_distance$Date_Distance_I18n_En$circa, 'less than', _justinmimbs$elm_date_extra$Date_Extra$Minute, _p4._0);
		case 'XMinutes':
			return A2(_alpacaaa$elm_date_distance$Date_Distance_I18n_En$exact, _justinmimbs$elm_date_extra$Date_Extra$Minute, _p4._0);
		case 'AboutXHours':
			return A3(_alpacaaa$elm_date_distance$Date_Distance_I18n_En$circa, 'about', _justinmimbs$elm_date_extra$Date_Extra$Hour, _p4._0);
		case 'XDays':
			return A2(_alpacaaa$elm_date_distance$Date_Distance_I18n_En$exact, _justinmimbs$elm_date_extra$Date_Extra$Day, _p4._0);
		case 'AboutXMonths':
			return A3(_alpacaaa$elm_date_distance$Date_Distance_I18n_En$circa, 'about', _justinmimbs$elm_date_extra$Date_Extra$Month, _p4._0);
		case 'XMonths':
			return A2(_alpacaaa$elm_date_distance$Date_Distance_I18n_En$exact, _justinmimbs$elm_date_extra$Date_Extra$Month, _p4._0);
		case 'AboutXYears':
			return A3(_alpacaaa$elm_date_distance$Date_Distance_I18n_En$circa, 'about', _justinmimbs$elm_date_extra$Date_Extra$Year, _p4._0);
		case 'OverXYears':
			return A3(_alpacaaa$elm_date_distance$Date_Distance_I18n_En$circa, 'over', _justinmimbs$elm_date_extra$Date_Extra$Year, _p4._0);
		default:
			return A3(_alpacaaa$elm_date_distance$Date_Distance_I18n_En$circa, 'almost', _justinmimbs$elm_date_extra$Date_Extra$Year, _p4._0);
	}
};
var _alpacaaa$elm_date_distance$Date_Distance_I18n_En$locale = F3(
	function (_p5, order, distance) {
		var _p6 = _p5;
		var result = _alpacaaa$elm_date_distance$Date_Distance_I18n_En$locale_(distance);
		return _p6.addSuffix ? (_elm_lang$core$Native_Utils.eq(order, _elm_lang$core$Basics$LT) ? A2(_elm_lang$core$Basics_ops['++'], 'in ', result) : A2(_elm_lang$core$Basics_ops['++'], result, ' ago')) : result;
	});
var _alpacaaa$elm_date_distance$Date_Distance_I18n_En$LocaleConfig = function (a) {
	return {addSuffix: a};
};

var _alpacaaa$elm_date_distance$Date_Distance$upToOneDay = function (minutes) {
	var hours = _elm_lang$core$Basics$round(
		_elm_lang$core$Basics$toFloat(minutes) / 60);
	return _alpacaaa$elm_date_distance$Date_Distance_Types$AboutXHours(hours);
};
var _alpacaaa$elm_date_distance$Date_Distance$upToOneMinute = function (seconds) {
	return (_elm_lang$core$Native_Utils.cmp(seconds, 5) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$LessThanXSeconds(5) : ((_elm_lang$core$Native_Utils.cmp(seconds, 10) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$LessThanXSeconds(10) : ((_elm_lang$core$Native_Utils.cmp(seconds, 20) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$LessThanXSeconds(20) : ((_elm_lang$core$Native_Utils.cmp(seconds, 40) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$HalfAMinute : ((_elm_lang$core$Native_Utils.cmp(seconds, 60) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$LessThanXMinutes(1) : _alpacaaa$elm_date_distance$Date_Distance_Types$XMinutes(1)))));
};
var _alpacaaa$elm_date_distance$Date_Distance$defaultConfig = {
	locale: _alpacaaa$elm_date_distance$Date_Distance_I18n_En$locale(
		{addSuffix: false}),
	includeSeconds: false
};
var _alpacaaa$elm_date_distance$Date_Distance$minutes_in_two_months = 86400;
var _alpacaaa$elm_date_distance$Date_Distance$minutes_in_month = 43200;
var _alpacaaa$elm_date_distance$Date_Distance$upToTwoMonths = function (minutes) {
	var months = _elm_lang$core$Basics$round(
		_elm_lang$core$Basics$toFloat(minutes) / _alpacaaa$elm_date_distance$Date_Distance$minutes_in_month);
	return _alpacaaa$elm_date_distance$Date_Distance_Types$AboutXMonths(months);
};
var _alpacaaa$elm_date_distance$Date_Distance$upToOneYear = function (minutes) {
	var nearestMonth = _elm_lang$core$Basics$round(
		_elm_lang$core$Basics$toFloat(minutes) / _alpacaaa$elm_date_distance$Date_Distance$minutes_in_month);
	return _alpacaaa$elm_date_distance$Date_Distance_Types$XMonths(nearestMonth);
};
var _alpacaaa$elm_date_distance$Date_Distance$moreThanTwoMonths = F3(
	function (minutes, d1, d2) {
		var months = A3(_justinmimbs$elm_date_extra$Date_Extra$diff, _justinmimbs$elm_date_extra$Date_Extra$Month, d1, d2);
		if (_elm_lang$core$Native_Utils.cmp(months, 12) < 0) {
			return _alpacaaa$elm_date_distance$Date_Distance$upToOneYear(minutes);
		} else {
			var years = _elm_lang$core$Basics$floor(
				_elm_lang$core$Basics$toFloat(months) / 12);
			var monthsSinceStartOfYear = A2(_elm_lang$core$Basics_ops['%'], months, 12);
			return (_elm_lang$core$Native_Utils.cmp(monthsSinceStartOfYear, 3) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$AboutXYears(years) : ((_elm_lang$core$Native_Utils.cmp(monthsSinceStartOfYear, 9) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$OverXYears(years) : _alpacaaa$elm_date_distance$Date_Distance_Types$AlmostXYears(years + 1));
		}
	});
var _alpacaaa$elm_date_distance$Date_Distance$minutes_in_almost_two_days = 2520;
var _alpacaaa$elm_date_distance$Date_Distance$minutes_in_day = 1440;
var _alpacaaa$elm_date_distance$Date_Distance$upToOneMonth = function (minutes) {
	var days = _elm_lang$core$Basics$round(
		_elm_lang$core$Basics$toFloat(minutes) / _alpacaaa$elm_date_distance$Date_Distance$minutes_in_day);
	return _alpacaaa$elm_date_distance$Date_Distance_Types$XDays(days);
};
var _alpacaaa$elm_date_distance$Date_Distance$calculateDistance = F3(
	function (includeSeconds, d1, d2) {
		var offset = _justinmimbs$elm_date_extra$Date_Extra$offsetFromUtc(d1) - _justinmimbs$elm_date_extra$Date_Extra$offsetFromUtc(d2);
		var seconds = A3(_justinmimbs$elm_date_extra$Date_Extra$diff, _justinmimbs$elm_date_extra$Date_Extra$Second, d1, d2);
		var minutes = _elm_lang$core$Basics$round(
			_elm_lang$core$Basics$toFloat(seconds) / 60) - offset;
		return (includeSeconds && (_elm_lang$core$Native_Utils.cmp(minutes, 2) < 0)) ? _alpacaaa$elm_date_distance$Date_Distance$upToOneMinute(seconds) : (_elm_lang$core$Native_Utils.eq(minutes, 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$LessThanXMinutes(1) : ((_elm_lang$core$Native_Utils.cmp(minutes, 2) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$XMinutes(minutes) : ((_elm_lang$core$Native_Utils.cmp(minutes, 45) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$XMinutes(minutes) : ((_elm_lang$core$Native_Utils.cmp(minutes, 90) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$AboutXHours(1) : ((_elm_lang$core$Native_Utils.cmp(minutes, _alpacaaa$elm_date_distance$Date_Distance$minutes_in_day) < 0) ? _alpacaaa$elm_date_distance$Date_Distance$upToOneDay(minutes) : ((_elm_lang$core$Native_Utils.cmp(minutes, _alpacaaa$elm_date_distance$Date_Distance$minutes_in_almost_two_days) < 0) ? _alpacaaa$elm_date_distance$Date_Distance_Types$XDays(1) : ((_elm_lang$core$Native_Utils.cmp(minutes, _alpacaaa$elm_date_distance$Date_Distance$minutes_in_month) < 0) ? _alpacaaa$elm_date_distance$Date_Distance$upToOneMonth(minutes) : ((_elm_lang$core$Native_Utils.cmp(minutes, _alpacaaa$elm_date_distance$Date_Distance$minutes_in_two_months) < 0) ? _alpacaaa$elm_date_distance$Date_Distance$upToTwoMonths(minutes) : A3(_alpacaaa$elm_date_distance$Date_Distance$moreThanTwoMonths, minutes, d1, d2)))))))));
	});
var _alpacaaa$elm_date_distance$Date_Distance$inWordsWithConfig = F3(
	function (_p0, d1, d2) {
		var _p1 = _p0;
		var order = A2(_justinmimbs$elm_date_extra$Date_Extra$compare, d1, d2);
		var _p2 = _elm_lang$core$Native_Utils.eq(order, _elm_lang$core$Basics$LT) ? {ctor: '_Tuple2', _0: d1, _1: d2} : {ctor: '_Tuple2', _0: d2, _1: d1};
		var fst = _p2._0;
		var snd = _p2._1;
		var distance = A3(_alpacaaa$elm_date_distance$Date_Distance$calculateDistance, _p1.includeSeconds, fst, snd);
		var localize = _p1.locale(order);
		return localize(distance);
	});
var _alpacaaa$elm_date_distance$Date_Distance$inWords = _alpacaaa$elm_date_distance$Date_Distance$inWordsWithConfig(_alpacaaa$elm_date_distance$Date_Distance$defaultConfig);

var _cuducos$elm_format_number$FormatNumber_Locales$Locale = F3(
	function (a, b, c) {
		return {decimals: a, thousandSeparator: b, decimalSeparator: c};
	});
var _cuducos$elm_format_number$FormatNumber_Locales$frenchLocale = A3(_cuducos$elm_format_number$FormatNumber_Locales$Locale, 3, ' ', ',');
var _cuducos$elm_format_number$FormatNumber_Locales$spanishLocale = A3(_cuducos$elm_format_number$FormatNumber_Locales$Locale, 3, '.', ',');
var _cuducos$elm_format_number$FormatNumber_Locales$usLocale = A3(_cuducos$elm_format_number$FormatNumber_Locales$Locale, 2, ',', '.');

var _myrho$elm_round$Round$funNum = F3(
	function (fun, s, fl) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			1 / 0,
			_elm_lang$core$Result$toMaybe(
				_elm_lang$core$String$toFloat(
					A2(fun, s, fl))));
	});
var _myrho$elm_round$Round$splitComma = function (str) {
	var _p0 = A2(_elm_lang$core$String$split, '.', str);
	if (_p0.ctor === '::') {
		if (_p0._1.ctor === '::') {
			return {ctor: '_Tuple2', _0: _p0._0, _1: _p0._1._0};
		} else {
			return {ctor: '_Tuple2', _0: _p0._0, _1: '0'};
		}
	} else {
		return {ctor: '_Tuple2', _0: '0', _1: '0'};
	}
};
var _myrho$elm_round$Round$toDecimal = function (fl) {
	var _p1 = A2(
		_elm_lang$core$String$split,
		'e',
		_elm_lang$core$Basics$toString(fl));
	if (_p1.ctor === '::') {
		if (_p1._1.ctor === '::') {
			var _p4 = _p1._1._0;
			var _p2 = function () {
				var hasSign = _elm_lang$core$Native_Utils.cmp(fl, 0) < 0;
				var _p3 = _myrho$elm_round$Round$splitComma(_p1._0);
				var b = _p3._0;
				var a = _p3._1;
				return {
					ctor: '_Tuple3',
					_0: hasSign ? '-' : '',
					_1: hasSign ? A2(_elm_lang$core$String$dropLeft, 1, b) : b,
					_2: a
				};
			}();
			var sign = _p2._0;
			var before = _p2._1;
			var after = _p2._2;
			var e = A2(
				_elm_lang$core$Maybe$withDefault,
				0,
				_elm_lang$core$Result$toMaybe(
					_elm_lang$core$String$toInt(
						A2(_elm_lang$core$String$startsWith, '+', _p4) ? A2(_elm_lang$core$String$dropLeft, 1, _p4) : _p4)));
			var newBefore = (_elm_lang$core$Native_Utils.cmp(e, 0) > -1) ? before : ((_elm_lang$core$Native_Utils.cmp(
				_elm_lang$core$Basics$abs(e),
				_elm_lang$core$String$length(before)) < 0) ? A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$String$left,
					_elm_lang$core$String$length(before) - _elm_lang$core$Basics$abs(e),
					before),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'.',
					A2(
						_elm_lang$core$String$right,
						_elm_lang$core$Basics$abs(e),
						before))) : A2(
				_elm_lang$core$Basics_ops['++'],
				'0.',
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$String$repeat,
						_elm_lang$core$Basics$abs(e) - _elm_lang$core$String$length(before),
						'0'),
					before)));
			var newAfter = (_elm_lang$core$Native_Utils.cmp(e, 0) < 1) ? after : ((_elm_lang$core$Native_Utils.cmp(
				e,
				_elm_lang$core$String$length(after)) < 0) ? A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_elm_lang$core$String$left, e, after),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'.',
					A2(
						_elm_lang$core$String$right,
						_elm_lang$core$String$length(after) - e,
						after))) : A2(
				_elm_lang$core$Basics_ops['++'],
				after,
				A2(
					_elm_lang$core$String$repeat,
					e - _elm_lang$core$String$length(after),
					'0')));
			return A2(
				_elm_lang$core$Basics_ops['++'],
				sign,
				A2(_elm_lang$core$Basics_ops['++'], newBefore, newAfter));
		} else {
			return _p1._0;
		}
	} else {
		return '';
	}
};
var _myrho$elm_round$Round$truncate = function (n) {
	return (_elm_lang$core$Native_Utils.cmp(n, 0) < 0) ? _elm_lang$core$Basics$ceiling(n) : _elm_lang$core$Basics$floor(n);
};
var _myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if (_elm_lang$core$Native_Utils.eq(s, 0)) {
			return _elm_lang$core$Basics$toString(
				functor(fl));
		} else {
			if (_elm_lang$core$Native_Utils.cmp(s, 0) < 0) {
				return function (r) {
					return (!_elm_lang$core$Native_Utils.eq(r, '0')) ? A2(
						_elm_lang$core$Basics_ops['++'],
						r,
						A2(
							_elm_lang$core$String$repeat,
							_elm_lang$core$Basics$abs(s),
							'0')) : r;
				}(
					A3(
						_myrho$elm_round$Round$roundFun,
						functor,
						0,
						A2(
							F2(
								function (x, y) {
									return x / y;
								}),
							fl,
							A2(
								F2(
									function (x, y) {
										return Math.pow(x, y);
									}),
								10,
								_elm_lang$core$Basics$abs(
									_elm_lang$core$Basics$toFloat(s))))));
			} else {
				var dd = (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? 2 : 1;
				var n = (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? -1 : 1;
				var e = Math.pow(10, s);
				var _p5 = _myrho$elm_round$Round$splitComma(
					_myrho$elm_round$Round$toDecimal(fl));
				var before = _p5._0;
				var after = _p5._1;
				var a = A3(
					_elm_lang$core$String$padRight,
					s + 1,
					_elm_lang$core$Native_Utils.chr('0'),
					after);
				var b = A2(_elm_lang$core$String$left, s, a);
				var c = A2(_elm_lang$core$String$dropLeft, s, a);
				var f = functor(
					A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Basics$toFloat(e),
						_elm_lang$core$Result$toMaybe(
							_elm_lang$core$String$toFloat(
								A2(
									_elm_lang$core$Basics_ops['++'],
									(_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? '-' : '',
									A2(
										_elm_lang$core$Basics_ops['++'],
										'1',
										A2(
											_elm_lang$core$Basics_ops['++'],
											b,
											A2(_elm_lang$core$Basics_ops['++'], '.', c))))))));
				var g = A2(
					_elm_lang$core$String$dropLeft,
					dd,
					_elm_lang$core$Basics$toString(f));
				var h = _myrho$elm_round$Round$truncate(fl) + (_elm_lang$core$Native_Utils.eq(f - (e * n), e * n) ? ((_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? -1 : 1) : 0);
				var j = _elm_lang$core$Basics$toString(h);
				var i = (_elm_lang$core$Native_Utils.eq(j, '0') && ((!_elm_lang$core$Native_Utils.eq(f - (e * n), 0)) && ((_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) && (_elm_lang$core$Native_Utils.cmp(fl, -1) > 0)))) ? A2(_elm_lang$core$Basics_ops['++'], '-', j) : j;
				return A2(
					_elm_lang$core$Basics_ops['++'],
					i,
					A2(_elm_lang$core$Basics_ops['++'], '.', g));
			}
		}
	});
var _myrho$elm_round$Round$round = _myrho$elm_round$Round$roundFun(_elm_lang$core$Basics$round);
var _myrho$elm_round$Round$roundNum = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$round);
var _myrho$elm_round$Round$ceiling = _myrho$elm_round$Round$roundFun(_elm_lang$core$Basics$ceiling);
var _myrho$elm_round$Round$ceilingNum = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$ceiling);
var _myrho$elm_round$Round$floor = _myrho$elm_round$Round$roundFun(_elm_lang$core$Basics$floor);
var _myrho$elm_round$Round$floorCom = F2(
	function (s, fl) {
		return (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? A2(_myrho$elm_round$Round$ceiling, s, fl) : A2(_myrho$elm_round$Round$floor, s, fl);
	});
var _myrho$elm_round$Round$floorNumCom = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$floorCom);
var _myrho$elm_round$Round$ceilingCom = F2(
	function (s, fl) {
		return (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? A2(_myrho$elm_round$Round$floor, s, fl) : A2(_myrho$elm_round$Round$ceiling, s, fl);
	});
var _myrho$elm_round$Round$ceilingNumCom = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$ceilingCom);
var _myrho$elm_round$Round$floorNum = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$floor);
var _myrho$elm_round$Round$roundCom = _myrho$elm_round$Round$roundFun(
	function (fl) {
		var dec = fl - _elm_lang$core$Basics$toFloat(
			_myrho$elm_round$Round$truncate(fl));
		return (_elm_lang$core$Native_Utils.cmp(dec, 0.5) > -1) ? _elm_lang$core$Basics$ceiling(fl) : ((_elm_lang$core$Native_Utils.cmp(dec, -0.5) < 1) ? _elm_lang$core$Basics$floor(fl) : _elm_lang$core$Basics$round(fl));
	});
var _myrho$elm_round$Round$roundNumCom = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$roundCom);

var _cuducos$elm_format_number$Helpers$stringfy = F2(
	function (locale, formatted) {
		var decimals = function () {
			var _p0 = formatted.decimals;
			if (_p0.ctor === 'Just') {
				return A2(_elm_lang$core$Basics_ops['++'], locale.decimalSeparator, _p0._0);
			} else {
				return '';
			}
		}();
		return _elm_lang$core$String$concat(
			{
				ctor: '::',
				_0: A2(_elm_lang$core$Maybe$withDefault, '', formatted.prefix),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$String$join, locale.thousandSeparator, formatted.integers),
					_1: {
						ctor: '::',
						_0: decimals,
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _cuducos$elm_format_number$Helpers$splitThousands = function (integers) {
	var reversedSplitThousands = function (value) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$String$length(value),
			3) > 0) ? A2(
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			A2(_elm_lang$core$String$right, 3, value),
			reversedSplitThousands(
				A2(_elm_lang$core$String$dropRight, 3, value))) : {
			ctor: '::',
			_0: value,
			_1: {ctor: '[]'}
		};
	};
	return _elm_lang$core$List$reverse(
		reversedSplitThousands(integers));
};
var _cuducos$elm_format_number$Helpers$addPrefix = function (formatted) {
	var onlyZeros = A2(
		_elm_lang$core$String$all,
		function ($char) {
			return _elm_lang$core$Native_Utils.eq(
				$char,
				_elm_lang$core$Native_Utils.chr('0'));
		},
		_elm_lang$core$String$concat(
			A2(
				_elm_lang$core$List$append,
				formatted.integers,
				_elm_lang$core$List$singleton(
					A2(_elm_lang$core$Maybe$withDefault, '', formatted.decimals)))));
	var isPositive = _elm_lang$core$Native_Utils.cmp(formatted.original, 0) > -1;
	var prefix = (isPositive || onlyZeros) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just('−');
	return _elm_lang$core$Native_Utils.update(
		formatted,
		{prefix: prefix});
};
var _cuducos$elm_format_number$Helpers$FormattedNumber = F4(
	function (a, b, c, d) {
		return {original: a, integers: b, decimals: c, prefix: d};
	});
var _cuducos$elm_format_number$Helpers$parse = F2(
	function (decimalDigits, original) {
		var parts = A2(
			_elm_lang$core$String$split,
			'.',
			A2(_myrho$elm_round$Round$round, decimalDigits, original));
		var integers = _cuducos$elm_format_number$Helpers$splitThousands(
			A2(
				_elm_lang$core$String$filter,
				_elm_lang$core$Char$isDigit,
				A2(
					_elm_lang$core$Maybe$withDefault,
					'0',
					_elm_lang$core$List$head(parts))));
		var decimals = _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, 1, parts));
		return _cuducos$elm_format_number$Helpers$addPrefix(
			A4(_cuducos$elm_format_number$Helpers$FormattedNumber, original, integers, decimals, _elm_lang$core$Maybe$Nothing));
	});

var _cuducos$elm_format_number$FormatNumber$format = F2(
	function (locale, num) {
		return A2(
			_cuducos$elm_format_number$Helpers$stringfy,
			locale,
			A2(_cuducos$elm_format_number$Helpers$parse, locale.decimals, num));
	});

var _elm_lang$core$Set$foldr = F3(
	function (f, b, _p0) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (k, _p2, b) {
					return A2(f, k, b);
				}),
			b,
			_p1._0);
	});
var _elm_lang$core$Set$foldl = F3(
	function (f, b, _p3) {
		var _p4 = _p3;
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, _p5, b) {
					return A2(f, k, b);
				}),
			b,
			_p4._0);
	});
var _elm_lang$core$Set$toList = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$core$Dict$keys(_p7._0);
};
var _elm_lang$core$Set$size = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Dict$size(_p9._0);
};
var _elm_lang$core$Set$member = F2(
	function (k, _p10) {
		var _p11 = _p10;
		return A2(_elm_lang$core$Dict$member, k, _p11._0);
	});
var _elm_lang$core$Set$isEmpty = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$core$Dict$isEmpty(_p13._0);
};
var _elm_lang$core$Set$Set_elm_builtin = function (a) {
	return {ctor: 'Set_elm_builtin', _0: a};
};
var _elm_lang$core$Set$empty = _elm_lang$core$Set$Set_elm_builtin(_elm_lang$core$Dict$empty);
var _elm_lang$core$Set$singleton = function (k) {
	return _elm_lang$core$Set$Set_elm_builtin(
		A2(
			_elm_lang$core$Dict$singleton,
			k,
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Set$insert = F2(
	function (k, _p14) {
		var _p15 = _p14;
		return _elm_lang$core$Set$Set_elm_builtin(
			A3(
				_elm_lang$core$Dict$insert,
				k,
				{ctor: '_Tuple0'},
				_p15._0));
	});
var _elm_lang$core$Set$fromList = function (xs) {
	return A3(_elm_lang$core$List$foldl, _elm_lang$core$Set$insert, _elm_lang$core$Set$empty, xs);
};
var _elm_lang$core$Set$map = F2(
	function (f, s) {
		return _elm_lang$core$Set$fromList(
			A2(
				_elm_lang$core$List$map,
				f,
				_elm_lang$core$Set$toList(s)));
	});
var _elm_lang$core$Set$remove = F2(
	function (k, _p16) {
		var _p17 = _p16;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$remove, k, _p17._0));
	});
var _elm_lang$core$Set$union = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$union, _p20._0, _p21._0));
	});
var _elm_lang$core$Set$intersect = F2(
	function (_p23, _p22) {
		var _p24 = _p23;
		var _p25 = _p22;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$intersect, _p24._0, _p25._0));
	});
var _elm_lang$core$Set$diff = F2(
	function (_p27, _p26) {
		var _p28 = _p27;
		var _p29 = _p26;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$diff, _p28._0, _p29._0));
	});
var _elm_lang$core$Set$filter = F2(
	function (p, _p30) {
		var _p31 = _p30;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p32) {
						return p(k);
					}),
				_p31._0));
	});
var _elm_lang$core$Set$partition = F2(
	function (p, _p33) {
		var _p34 = _p33;
		var _p35 = A2(
			_elm_lang$core$Dict$partition,
			F2(
				function (k, _p36) {
					return p(k);
				}),
			_p34._0);
		var p1 = _p35._0;
		var p2 = _p35._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Set$Set_elm_builtin(p1),
			_1: _elm_lang$core$Set$Set_elm_builtin(p2)
		};
	});

var _elm_community$list_extra$List_Extra$greedyGroupsOfWithStep = F3(
	function (size, step, xs) {
		var okayXs = _elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(xs),
			0) > 0;
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		return (okayArgs && okayXs) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$greedyGroupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		var okayLength = _elm_lang$core$Native_Utils.eq(
			size,
			_elm_lang$core$List$length(group));
		return (okayArgs && okayLength) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$groupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$zip5 = _elm_lang$core$List$map5(
	F5(
		function (v0, v1, v2, v3, v4) {
			return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
		}));
var _elm_community$list_extra$List_Extra$zip4 = _elm_lang$core$List$map4(
	F4(
		function (v0, v1, v2, v3) {
			return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
		}));
var _elm_community$list_extra$List_Extra$zip3 = _elm_lang$core$List$map3(
	F3(
		function (v0, v1, v2) {
			return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
		}));
var _elm_community$list_extra$List_Extra$zip = _elm_lang$core$List$map2(
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}));
var _elm_community$list_extra$List_Extra$isPrefixOf = F2(
	function (prefix, xs) {
		var _p0 = {ctor: '_Tuple2', _0: prefix, _1: xs};
		if (_p0._0.ctor === '[]') {
			return true;
		} else {
			if (_p0._1.ctor === '[]') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.eq(_p0._0._0, _p0._1._0) && A2(_elm_community$list_extra$List_Extra$isPrefixOf, _p0._0._1, _p0._1._1);
			}
		}
	});
var _elm_community$list_extra$List_Extra$isSuffixOf = F2(
	function (suffix, xs) {
		return A2(
			_elm_community$list_extra$List_Extra$isPrefixOf,
			_elm_lang$core$List$reverse(suffix),
			_elm_lang$core$List$reverse(xs));
	});
var _elm_community$list_extra$List_Extra$selectSplit = function (xs) {
	var _p1 = xs;
	if (_p1.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p5 = _p1._1;
		var _p4 = _p1._0;
		return {
			ctor: '::',
			_0: {
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _p4,
				_2: _p5
			},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p2) {
					var _p3 = _p2;
					return {
						ctor: '_Tuple3',
						_0: {ctor: '::', _0: _p4, _1: _p3._0},
						_1: _p3._1,
						_2: _p3._2
					};
				},
				_elm_community$list_extra$List_Extra$selectSplit(_p5))
		};
	}
};
var _elm_community$list_extra$List_Extra$select = function (xs) {
	var _p6 = xs;
	if (_p6.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p10 = _p6._1;
		var _p9 = _p6._0;
		return {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _p9, _1: _p10},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p7) {
					var _p8 = _p7;
					return {
						ctor: '_Tuple2',
						_0: _p8._0,
						_1: {ctor: '::', _0: _p9, _1: _p8._1}
					};
				},
				_elm_community$list_extra$List_Extra$select(_p10))
		};
	}
};
var _elm_community$list_extra$List_Extra$tailsHelp = F2(
	function (e, list) {
		var _p11 = list;
		if (_p11.ctor === '::') {
			var _p12 = _p11._0;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: e, _1: _p12},
				_1: {ctor: '::', _0: _p12, _1: _p11._1}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _elm_community$list_extra$List_Extra$tails = A2(
	_elm_lang$core$List$foldr,
	_elm_community$list_extra$List_Extra$tailsHelp,
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$isInfixOf = F2(
	function (infix, xs) {
		return A2(
			_elm_lang$core$List$any,
			_elm_community$list_extra$List_Extra$isPrefixOf(infix),
			_elm_community$list_extra$List_Extra$tails(xs));
	});
var _elm_community$list_extra$List_Extra$inits = A2(
	_elm_lang$core$List$foldr,
	F2(
		function (e, acc) {
			return {
				ctor: '::',
				_0: {ctor: '[]'},
				_1: A2(
					_elm_lang$core$List$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(e),
					acc)
			};
		}),
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$groupWhileTransitively = F2(
	function (cmp, xs_) {
		var _p13 = xs_;
		if (_p13.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p13._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: {
						ctor: '::',
						_0: _p13._0,
						_1: {ctor: '[]'}
					},
					_1: {ctor: '[]'}
				};
			} else {
				var _p15 = _p13._0;
				var _p14 = A2(_elm_community$list_extra$List_Extra$groupWhileTransitively, cmp, _p13._1);
				if (_p14.ctor === '::') {
					return A2(cmp, _p15, _p13._1._0) ? {
						ctor: '::',
						_0: {ctor: '::', _0: _p15, _1: _p14._0},
						_1: _p14._1
					} : {
						ctor: '::',
						_0: {
							ctor: '::',
							_0: _p15,
							_1: {ctor: '[]'}
						},
						_1: _p14
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$stripPrefix = F2(
	function (prefix, xs) {
		var step = F2(
			function (e, m) {
				var _p16 = m;
				if (_p16.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_p16._0.ctor === '[]') {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						return _elm_lang$core$Native_Utils.eq(e, _p16._0._0) ? _elm_lang$core$Maybe$Just(_p16._0._1) : _elm_lang$core$Maybe$Nothing;
					}
				}
			});
		return A3(
			_elm_lang$core$List$foldl,
			step,
			_elm_lang$core$Maybe$Just(xs),
			prefix);
	});
var _elm_community$list_extra$List_Extra$dropWhileRight = function (p) {
	return A2(
		_elm_lang$core$List$foldr,
		F2(
			function (x, xs) {
				return (p(x) && _elm_lang$core$List$isEmpty(xs)) ? {ctor: '[]'} : {ctor: '::', _0: x, _1: xs};
			}),
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$takeWhileRight = function (p) {
	var step = F2(
		function (x, _p17) {
			var _p18 = _p17;
			var _p19 = _p18._0;
			return (p(x) && _p18._1) ? {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: x, _1: _p19},
				_1: true
			} : {ctor: '_Tuple2', _0: _p19, _1: false};
		});
	return function (_p20) {
		return _elm_lang$core$Tuple$first(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: {ctor: '[]'},
					_1: true
				},
				_p20));
	};
};
var _elm_community$list_extra$List_Extra$splitAt = F2(
	function (n, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_lang$core$List$take, n, xs),
			_1: A2(_elm_lang$core$List$drop, n, xs)
		};
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying_ = F3(
	function (listOflengths, list, accu) {
		groupsOfVarying_:
		while (true) {
			var _p21 = {ctor: '_Tuple2', _0: listOflengths, _1: list};
			if (((_p21.ctor === '_Tuple2') && (_p21._0.ctor === '::')) && (_p21._1.ctor === '::')) {
				var _p22 = A2(_elm_community$list_extra$List_Extra$splitAt, _p21._0._0, list);
				var head = _p22._0;
				var tail = _p22._1;
				var _v11 = _p21._0._1,
					_v12 = tail,
					_v13 = {ctor: '::', _0: head, _1: accu};
				listOflengths = _v11;
				list = _v12;
				accu = _v13;
				continue groupsOfVarying_;
			} else {
				return _elm_lang$core$List$reverse(accu);
			}
		}
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying = F2(
	function (listOflengths, list) {
		return A3(
			_elm_community$list_extra$List_Extra$groupsOfVarying_,
			listOflengths,
			list,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$unfoldr = F2(
	function (f, seed) {
		var _p23 = f(seed);
		if (_p23.ctor === 'Nothing') {
			return {ctor: '[]'};
		} else {
			return {
				ctor: '::',
				_0: _p23._0._0,
				_1: A2(_elm_community$list_extra$List_Extra$unfoldr, f, _p23._0._1)
			};
		}
	});
var _elm_community$list_extra$List_Extra$scanr1 = F2(
	function (f, xs_) {
		var _p24 = xs_;
		if (_p24.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p24._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: _p24._0,
					_1: {ctor: '[]'}
				};
			} else {
				var _p25 = A2(_elm_community$list_extra$List_Extra$scanr1, f, _p24._1);
				if (_p25.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, _p24._0, _p25._0),
						_1: _p25
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanr = F3(
	function (f, acc, xs_) {
		var _p26 = xs_;
		if (_p26.ctor === '[]') {
			return {
				ctor: '::',
				_0: acc,
				_1: {ctor: '[]'}
			};
		} else {
			var _p27 = A3(_elm_community$list_extra$List_Extra$scanr, f, acc, _p26._1);
			if (_p27.ctor === '::') {
				return {
					ctor: '::',
					_0: A2(f, _p26._0, _p27._0),
					_1: _p27
				};
			} else {
				return {ctor: '[]'};
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanl1 = F2(
	function (f, xs_) {
		var _p28 = xs_;
		if (_p28.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			return A3(_elm_lang$core$List$scanl, f, _p28._0, _p28._1);
		}
	});
var _elm_community$list_extra$List_Extra$indexedFoldr = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p29) {
				var _p30 = _p29;
				var _p31 = _p30._0;
				return {
					ctor: '_Tuple2',
					_0: _p31 - 1,
					_1: A3(func, _p31, x, _p30._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$length(list) - 1,
					_1: acc
				},
				list));
	});
var _elm_community$list_extra$List_Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p32) {
				var _p33 = _p32;
				var _p34 = _p33._0;
				return {
					ctor: '_Tuple2',
					_0: _p34 + 1,
					_1: A3(func, _p34, x, _p33._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				step,
				{ctor: '_Tuple2', _0: 0, _1: acc},
				list));
	});
var _elm_community$list_extra$List_Extra$foldr1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p35 = m;
						if (_p35.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, x, _p35._0);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldr, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$foldl1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p36 = m;
						if (_p36.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, _p36._0, x);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldl, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$interweaveHelp = F3(
	function (l1, l2, acc) {
		interweaveHelp:
		while (true) {
			var _p37 = {ctor: '_Tuple2', _0: l1, _1: l2};
			_v24_1:
			do {
				if (_p37._0.ctor === '::') {
					if (_p37._1.ctor === '::') {
						var _v25 = _p37._0._1,
							_v26 = _p37._1._1,
							_v27 = A2(
							_elm_lang$core$Basics_ops['++'],
							acc,
							{
								ctor: '::',
								_0: _p37._0._0,
								_1: {
									ctor: '::',
									_0: _p37._1._0,
									_1: {ctor: '[]'}
								}
							});
						l1 = _v25;
						l2 = _v26;
						acc = _v27;
						continue interweaveHelp;
					} else {
						break _v24_1;
					}
				} else {
					if (_p37._1.ctor === '[]') {
						break _v24_1;
					} else {
						return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._1);
					}
				}
			} while(false);
			return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._0);
		}
	});
var _elm_community$list_extra$List_Extra$interweave = F2(
	function (l1, l2) {
		return A3(
			_elm_community$list_extra$List_Extra$interweaveHelp,
			l1,
			l2,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$permutations = function (xs_) {
	var _p38 = xs_;
	if (_p38.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		var f = function (_p39) {
			var _p40 = _p39;
			return A2(
				_elm_lang$core$List$map,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(_p40._0),
				_elm_community$list_extra$List_Extra$permutations(_p40._1));
		};
		return A2(
			_elm_lang$core$List$concatMap,
			f,
			_elm_community$list_extra$List_Extra$select(_p38));
	}
};
var _elm_community$list_extra$List_Extra$isPermutationOf = F2(
	function (permut, xs) {
		return A2(
			_elm_lang$core$List$member,
			permut,
			_elm_community$list_extra$List_Extra$permutations(xs));
	});
var _elm_community$list_extra$List_Extra$subsequencesNonEmpty = function (xs) {
	var _p41 = xs;
	if (_p41.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p42 = _p41._0;
		var f = F2(
			function (ys, r) {
				return {
					ctor: '::',
					_0: ys,
					_1: {
						ctor: '::',
						_0: {ctor: '::', _0: _p42, _1: ys},
						_1: r
					}
				};
			});
		return {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _p42,
				_1: {ctor: '[]'}
			},
			_1: A3(
				_elm_lang$core$List$foldr,
				f,
				{ctor: '[]'},
				_elm_community$list_extra$List_Extra$subsequencesNonEmpty(_p41._1))
		};
	}
};
var _elm_community$list_extra$List_Extra$subsequences = function (xs) {
	return {
		ctor: '::',
		_0: {ctor: '[]'},
		_1: _elm_community$list_extra$List_Extra$subsequencesNonEmpty(xs)
	};
};
var _elm_community$list_extra$List_Extra$isSubsequenceOf = F2(
	function (subseq, xs) {
		return A2(
			_elm_lang$core$List$member,
			subseq,
			_elm_community$list_extra$List_Extra$subsequences(xs));
	});
var _elm_community$list_extra$List_Extra$transpose = function (ll) {
	transpose:
	while (true) {
		var _p43 = ll;
		if (_p43.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p43._0.ctor === '[]') {
				var _v32 = _p43._1;
				ll = _v32;
				continue transpose;
			} else {
				var _p44 = _p43._1;
				var tails = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$tail, _p44);
				var heads = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$head, _p44);
				return {
					ctor: '::',
					_0: {ctor: '::', _0: _p43._0._0, _1: heads},
					_1: _elm_community$list_extra$List_Extra$transpose(
						{ctor: '::', _0: _p43._0._1, _1: tails})
				};
			}
		}
	}
};
var _elm_community$list_extra$List_Extra$intercalate = function (xs) {
	return function (_p45) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$intersperse, xs, _p45));
	};
};
var _elm_community$list_extra$List_Extra$filterNot = F2(
	function (pred, list) {
		return A2(
			_elm_lang$core$List$filter,
			function (_p46) {
				return !pred(_p46);
			},
			list);
	});
var _elm_community$list_extra$List_Extra$removeAt = F2(
	function (index, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return l;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p47 = tail;
			if (_p47.ctor === 'Nothing') {
				return l;
			} else {
				return A2(_elm_lang$core$List$append, head, _p47._0);
			}
		}
	});
var _elm_community$list_extra$List_Extra$stableSortWith = F2(
	function (pred, list) {
		var predWithIndex = F2(
			function (_p49, _p48) {
				var _p50 = _p49;
				var _p51 = _p48;
				var result = A2(pred, _p50._0, _p51._0);
				var _p52 = result;
				if (_p52.ctor === 'EQ') {
					return A2(_elm_lang$core$Basics$compare, _p50._1, _p51._1);
				} else {
					return result;
				}
			});
		var listWithIndex = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, a) {
					return {ctor: '_Tuple2', _0: a, _1: i};
				}),
			list);
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(_elm_lang$core$List$sortWith, predWithIndex, listWithIndex));
	});
var _elm_community$list_extra$List_Extra$setAt = F3(
	function (index, value, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p53 = tail;
			if (_p53.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(
					A2(
						_elm_lang$core$List$append,
						head,
						{ctor: '::', _0: value, _1: _p53._0}));
			}
		}
	});
var _elm_community$list_extra$List_Extra$remove = F2(
	function (x, xs) {
		var _p54 = xs;
		if (_p54.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p56 = _p54._1;
			var _p55 = _p54._0;
			return _elm_lang$core$Native_Utils.eq(x, _p55) ? _p56 : {
				ctor: '::',
				_0: _p55,
				_1: A2(_elm_community$list_extra$List_Extra$remove, x, _p56)
			};
		}
	});
var _elm_community$list_extra$List_Extra$updateIfIndex = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, x) {
					return predicate(i) ? update(x) : x;
				}),
			list);
	});
var _elm_community$list_extra$List_Extra$updateAt = F3(
	function (index, update, list) {
		return ((_elm_lang$core$Native_Utils.cmp(index, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(
			index,
			_elm_lang$core$List$length(list)) > -1)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
			A3(
				_elm_community$list_extra$List_Extra$updateIfIndex,
				F2(
					function (x, y) {
						return _elm_lang$core$Native_Utils.eq(x, y);
					})(index),
				update,
				list));
	});
var _elm_community$list_extra$List_Extra$updateIf = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$map,
			function (item) {
				return predicate(item) ? update(item) : item;
			},
			list);
	});
var _elm_community$list_extra$List_Extra$replaceIf = F3(
	function (predicate, replacement, list) {
		return A3(
			_elm_community$list_extra$List_Extra$updateIf,
			predicate,
			_elm_lang$core$Basics$always(replacement),
			list);
	});
var _elm_community$list_extra$List_Extra$findIndices = function (p) {
	return function (_p57) {
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(
				_elm_lang$core$List$filter,
				function (_p58) {
					var _p59 = _p58;
					return p(_p59._1);
				},
				A2(
					_elm_lang$core$List$indexedMap,
					F2(
						function (v0, v1) {
							return {ctor: '_Tuple2', _0: v0, _1: v1};
						}),
					_p57)));
	};
};
var _elm_community$list_extra$List_Extra$findIndex = function (p) {
	return function (_p60) {
		return _elm_lang$core$List$head(
			A2(_elm_community$list_extra$List_Extra$findIndices, p, _p60));
	};
};
var _elm_community$list_extra$List_Extra$splitWhen = F2(
	function (predicate, list) {
		return A2(
			_elm_lang$core$Maybe$map,
			function (i) {
				return A2(_elm_community$list_extra$List_Extra$splitAt, i, list);
			},
			A2(_elm_community$list_extra$List_Extra$findIndex, predicate, list));
	});
var _elm_community$list_extra$List_Extra$elemIndices = function (x) {
	return _elm_community$list_extra$List_Extra$findIndices(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$elemIndex = function (x) {
	return _elm_community$list_extra$List_Extra$findIndex(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			var _p61 = list;
			if (_p61.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p62 = _p61._0;
				if (predicate(_p62)) {
					return _elm_lang$core$Maybe$Just(_p62);
				} else {
					var _v41 = predicate,
						_v42 = _p61._1;
					predicate = _v41;
					list = _v42;
					continue find;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$notMember = function (x) {
	return function (_p63) {
		return !A2(_elm_lang$core$List$member, x, _p63);
	};
};
var _elm_community$list_extra$List_Extra$andThen = _elm_lang$core$List$concatMap;
var _elm_community$list_extra$List_Extra$lift2 = F3(
	function (f, la, lb) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return {
							ctor: '::',
							_0: A2(f, a, b),
							_1: {ctor: '[]'}
						};
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift3 = F4(
	function (f, la, lb, lc) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return {
									ctor: '::',
									_0: A3(f, a, b, c),
									_1: {ctor: '[]'}
								};
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift4 = F5(
	function (f, la, lb, lc, ld) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return A2(
									_elm_community$list_extra$List_Extra$andThen,
									function (d) {
										return {
											ctor: '::',
											_0: A4(f, a, b, c, d),
											_1: {ctor: '[]'}
										};
									},
									ld);
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$andMap = F2(
	function (l, fl) {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (x, y) {
					return x(y);
				}),
			fl,
			l);
	});
var _elm_community$list_extra$List_Extra$uniqueHelp = F3(
	function (f, existing, remaining) {
		uniqueHelp:
		while (true) {
			var _p64 = remaining;
			if (_p64.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				var _p66 = _p64._1;
				var _p65 = _p64._0;
				var computedFirst = f(_p65);
				if (A2(_elm_lang$core$Set$member, computedFirst, existing)) {
					var _v44 = f,
						_v45 = existing,
						_v46 = _p66;
					f = _v44;
					existing = _v45;
					remaining = _v46;
					continue uniqueHelp;
				} else {
					return {
						ctor: '::',
						_0: _p65,
						_1: A3(
							_elm_community$list_extra$List_Extra$uniqueHelp,
							f,
							A2(_elm_lang$core$Set$insert, computedFirst, existing),
							_p66)
					};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$uniqueBy = F2(
	function (f, list) {
		return A3(_elm_community$list_extra$List_Extra$uniqueHelp, f, _elm_lang$core$Set$empty, list);
	});
var _elm_community$list_extra$List_Extra$allDifferentBy = F2(
	function (f, list) {
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$List$length(list),
			_elm_lang$core$List$length(
				A2(_elm_community$list_extra$List_Extra$uniqueBy, f, list)));
	});
var _elm_community$list_extra$List_Extra$allDifferent = function (list) {
	return A2(_elm_community$list_extra$List_Extra$allDifferentBy, _elm_lang$core$Basics$identity, list);
};
var _elm_community$list_extra$List_Extra$unique = function (list) {
	return A3(_elm_community$list_extra$List_Extra$uniqueHelp, _elm_lang$core$Basics$identity, _elm_lang$core$Set$empty, list);
};
var _elm_community$list_extra$List_Extra$dropWhile = F2(
	function (predicate, list) {
		dropWhile:
		while (true) {
			var _p67 = list;
			if (_p67.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				if (predicate(_p67._0)) {
					var _v48 = predicate,
						_v49 = _p67._1;
					predicate = _v48;
					list = _v49;
					continue dropWhile;
				} else {
					return list;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				var _p68 = list;
				if (_p68.ctor === '[]') {
					return _elm_lang$core$List$reverse(memo);
				} else {
					var _p69 = _p68._0;
					if (predicate(_p69)) {
						var _v51 = {ctor: '::', _0: _p69, _1: memo},
							_v52 = _p68._1;
						memo = _v51;
						list = _v52;
						continue takeWhileMemo;
					} else {
						return _elm_lang$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$span = F2(
	function (p, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_community$list_extra$List_Extra$takeWhile, p, xs),
			_1: A2(_elm_community$list_extra$List_Extra$dropWhile, p, xs)
		};
	});
var _elm_community$list_extra$List_Extra$break = function (p) {
	return _elm_community$list_extra$List_Extra$span(
		function (_p70) {
			return !p(_p70);
		});
};
var _elm_community$list_extra$List_Extra$groupWhile = F2(
	function (eq, xs_) {
		var _p71 = xs_;
		if (_p71.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p73 = _p71._0;
			var _p72 = A2(
				_elm_community$list_extra$List_Extra$span,
				eq(_p73),
				_p71._1);
			var ys = _p72._0;
			var zs = _p72._1;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: _p73, _1: ys},
				_1: A2(_elm_community$list_extra$List_Extra$groupWhile, eq, zs)
			};
		}
	});
var _elm_community$list_extra$List_Extra$group = _elm_community$list_extra$List_Extra$groupWhile(
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		}));
var _elm_community$list_extra$List_Extra$minimumBy = F2(
	function (f, ls) {
		var minBy = F2(
			function (x, _p74) {
				var _p75 = _p74;
				var _p76 = _p75._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p76) < 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p75._0, _1: _p76};
			});
		var _p77 = ls;
		if (_p77.ctor === '::') {
			if (_p77._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p77._0);
			} else {
				var _p78 = _p77._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							minBy,
							{
								ctor: '_Tuple2',
								_0: _p78,
								_1: f(_p78)
							},
							_p77._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$maximumBy = F2(
	function (f, ls) {
		var maxBy = F2(
			function (x, _p79) {
				var _p80 = _p79;
				var _p81 = _p80._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p81) > 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p80._0, _1: _p81};
			});
		var _p82 = ls;
		if (_p82.ctor === '::') {
			if (_p82._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p82._0);
			} else {
				var _p83 = _p82._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							maxBy,
							{
								ctor: '_Tuple2',
								_0: _p83,
								_1: f(_p83)
							},
							_p82._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$uncons = function (xs) {
	var _p84 = xs;
	if (_p84.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: _p84._0, _1: _p84._1});
	}
};
var _elm_community$list_extra$List_Extra$swapAt = F3(
	function (index1, index2, l) {
		swapAt:
		while (true) {
			if (_elm_lang$core$Native_Utils.eq(index1, index2)) {
				return _elm_lang$core$Maybe$Just(l);
			} else {
				if (_elm_lang$core$Native_Utils.cmp(index1, index2) > 0) {
					var _v59 = index2,
						_v60 = index1,
						_v61 = l;
					index1 = _v59;
					index2 = _v60;
					l = _v61;
					continue swapAt;
				} else {
					if (_elm_lang$core$Native_Utils.cmp(index1, 0) < 0) {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						var _p85 = A2(_elm_community$list_extra$List_Extra$splitAt, index1, l);
						var part1 = _p85._0;
						var tail1 = _p85._1;
						var _p86 = A2(_elm_community$list_extra$List_Extra$splitAt, index2 - index1, tail1);
						var head2 = _p86._0;
						var tail2 = _p86._1;
						return A3(
							_elm_lang$core$Maybe$map2,
							F2(
								function (_p88, _p87) {
									var _p89 = _p88;
									var _p90 = _p87;
									return _elm_lang$core$List$concat(
										{
											ctor: '::',
											_0: part1,
											_1: {
												ctor: '::',
												_0: {ctor: '::', _0: _p90._0, _1: _p89._1},
												_1: {
													ctor: '::',
													_0: {ctor: '::', _0: _p89._0, _1: _p90._1},
													_1: {ctor: '[]'}
												}
											}
										});
								}),
							_elm_community$list_extra$List_Extra$uncons(head2),
							_elm_community$list_extra$List_Extra$uncons(tail2));
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$iterate = F2(
	function (f, x) {
		var _p91 = f(x);
		if (_p91.ctor === 'Just') {
			return {
				ctor: '::',
				_0: x,
				_1: A2(_elm_community$list_extra$List_Extra$iterate, f, _p91._0)
			};
		} else {
			return {
				ctor: '::',
				_0: x,
				_1: {ctor: '[]'}
			};
		}
	});
var _elm_community$list_extra$List_Extra$getAt = F2(
	function (idx, xs) {
		return (_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, idx, xs));
	});
var _elm_community$list_extra$List_Extra_ops = _elm_community$list_extra$List_Extra_ops || {};
_elm_community$list_extra$List_Extra_ops['!!'] = _elm_lang$core$Basics$flip(_elm_community$list_extra$List_Extra$getAt);
var _elm_community$list_extra$List_Extra$init = function () {
	var maybe = F2(
		function (d, f) {
			return function (_p92) {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					d,
					A2(_elm_lang$core$Maybe$map, f, _p92));
			};
		});
	return A2(
		_elm_lang$core$List$foldr,
		function (x) {
			return function (_p93) {
				return _elm_lang$core$Maybe$Just(
					A3(
						maybe,
						{ctor: '[]'},
						F2(
							function (x, y) {
								return {ctor: '::', _0: x, _1: y};
							})(x),
						_p93));
			};
		},
		_elm_lang$core$Maybe$Nothing);
}();
var _elm_community$list_extra$List_Extra$last = _elm_community$list_extra$List_Extra$foldl1(
	_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

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

var _evancz$url_parser$UrlParser$toKeyValuePair = function (segment) {
	var _p0 = A2(_elm_lang$core$String$split, '=', segment);
	if (((_p0.ctor === '::') && (_p0._1.ctor === '::')) && (_p0._1._1.ctor === '[]')) {
		return A3(
			_elm_lang$core$Maybe$map2,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			_elm_lang$http$Http$decodeUri(_p0._0),
			_elm_lang$http$Http$decodeUri(_p0._1._0));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _evancz$url_parser$UrlParser$parseParams = function (queryString) {
	return _elm_lang$core$Dict$fromList(
		A2(
			_elm_lang$core$List$filterMap,
			_evancz$url_parser$UrlParser$toKeyValuePair,
			A2(
				_elm_lang$core$String$split,
				'&',
				A2(_elm_lang$core$String$dropLeft, 1, queryString))));
};
var _evancz$url_parser$UrlParser$splitUrl = function (url) {
	var _p1 = A2(_elm_lang$core$String$split, '/', url);
	if ((_p1.ctor === '::') && (_p1._0 === '')) {
		return _p1._1;
	} else {
		return _p1;
	}
};
var _evancz$url_parser$UrlParser$parseHelp = function (states) {
	parseHelp:
	while (true) {
		var _p2 = states;
		if (_p2.ctor === '[]') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var _p4 = _p2._0;
			var _p3 = _p4.unvisited;
			if (_p3.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p4.value);
			} else {
				if ((_p3._0 === '') && (_p3._1.ctor === '[]')) {
					return _elm_lang$core$Maybe$Just(_p4.value);
				} else {
					var _v4 = _p2._1;
					states = _v4;
					continue parseHelp;
				}
			}
		}
	}
};
var _evancz$url_parser$UrlParser$parse = F3(
	function (_p5, url, params) {
		var _p6 = _p5;
		return _evancz$url_parser$UrlParser$parseHelp(
			_p6._0(
				{
					visited: {ctor: '[]'},
					unvisited: _evancz$url_parser$UrlParser$splitUrl(url),
					params: params,
					value: _elm_lang$core$Basics$identity
				}));
	});
var _evancz$url_parser$UrlParser$parseHash = F2(
	function (parser, location) {
		return A3(
			_evancz$url_parser$UrlParser$parse,
			parser,
			A2(_elm_lang$core$String$dropLeft, 1, location.hash),
			_evancz$url_parser$UrlParser$parseParams(location.search));
	});
var _evancz$url_parser$UrlParser$parsePath = F2(
	function (parser, location) {
		return A3(
			_evancz$url_parser$UrlParser$parse,
			parser,
			location.pathname,
			_evancz$url_parser$UrlParser$parseParams(location.search));
	});
var _evancz$url_parser$UrlParser$intParamHelp = function (maybeValue) {
	var _p7 = maybeValue;
	if (_p7.ctor === 'Nothing') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Result$toMaybe(
			_elm_lang$core$String$toInt(_p7._0));
	}
};
var _evancz$url_parser$UrlParser$mapHelp = F2(
	function (func, _p8) {
		var _p9 = _p8;
		return {
			visited: _p9.visited,
			unvisited: _p9.unvisited,
			params: _p9.params,
			value: func(_p9.value)
		};
	});
var _evancz$url_parser$UrlParser$State = F4(
	function (a, b, c, d) {
		return {visited: a, unvisited: b, params: c, value: d};
	});
var _evancz$url_parser$UrlParser$Parser = function (a) {
	return {ctor: 'Parser', _0: a};
};
var _evancz$url_parser$UrlParser$s = function (str) {
	return _evancz$url_parser$UrlParser$Parser(
		function (_p10) {
			var _p11 = _p10;
			var _p12 = _p11.unvisited;
			if (_p12.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				var _p13 = _p12._0;
				return _elm_lang$core$Native_Utils.eq(_p13, str) ? {
					ctor: '::',
					_0: A4(
						_evancz$url_parser$UrlParser$State,
						{ctor: '::', _0: _p13, _1: _p11.visited},
						_p12._1,
						_p11.params,
						_p11.value),
					_1: {ctor: '[]'}
				} : {ctor: '[]'};
			}
		});
};
var _evancz$url_parser$UrlParser$custom = F2(
	function (tipe, stringToSomething) {
		return _evancz$url_parser$UrlParser$Parser(
			function (_p14) {
				var _p15 = _p14;
				var _p16 = _p15.unvisited;
				if (_p16.ctor === '[]') {
					return {ctor: '[]'};
				} else {
					var _p18 = _p16._0;
					var _p17 = stringToSomething(_p18);
					if (_p17.ctor === 'Ok') {
						return {
							ctor: '::',
							_0: A4(
								_evancz$url_parser$UrlParser$State,
								{ctor: '::', _0: _p18, _1: _p15.visited},
								_p16._1,
								_p15.params,
								_p15.value(_p17._0)),
							_1: {ctor: '[]'}
						};
					} else {
						return {ctor: '[]'};
					}
				}
			});
	});
var _evancz$url_parser$UrlParser$string = A2(_evancz$url_parser$UrlParser$custom, 'STRING', _elm_lang$core$Result$Ok);
var _evancz$url_parser$UrlParser$int = A2(_evancz$url_parser$UrlParser$custom, 'NUMBER', _elm_lang$core$String$toInt);
var _evancz$url_parser$UrlParser_ops = _evancz$url_parser$UrlParser_ops || {};
_evancz$url_parser$UrlParser_ops['</>'] = F2(
	function (_p20, _p19) {
		var _p21 = _p20;
		var _p22 = _p19;
		return _evancz$url_parser$UrlParser$Parser(
			function (state) {
				return A2(
					_elm_lang$core$List$concatMap,
					_p22._0,
					_p21._0(state));
			});
	});
var _evancz$url_parser$UrlParser$map = F2(
	function (subValue, _p23) {
		var _p24 = _p23;
		return _evancz$url_parser$UrlParser$Parser(
			function (_p25) {
				var _p26 = _p25;
				return A2(
					_elm_lang$core$List$map,
					_evancz$url_parser$UrlParser$mapHelp(_p26.value),
					_p24._0(
						{visited: _p26.visited, unvisited: _p26.unvisited, params: _p26.params, value: subValue}));
			});
	});
var _evancz$url_parser$UrlParser$oneOf = function (parsers) {
	return _evancz$url_parser$UrlParser$Parser(
		function (state) {
			return A2(
				_elm_lang$core$List$concatMap,
				function (_p27) {
					var _p28 = _p27;
					return _p28._0(state);
				},
				parsers);
		});
};
var _evancz$url_parser$UrlParser$top = _evancz$url_parser$UrlParser$Parser(
	function (state) {
		return {
			ctor: '::',
			_0: state,
			_1: {ctor: '[]'}
		};
	});
var _evancz$url_parser$UrlParser_ops = _evancz$url_parser$UrlParser_ops || {};
_evancz$url_parser$UrlParser_ops['<?>'] = F2(
	function (_p30, _p29) {
		var _p31 = _p30;
		var _p32 = _p29;
		return _evancz$url_parser$UrlParser$Parser(
			function (state) {
				return A2(
					_elm_lang$core$List$concatMap,
					_p32._0,
					_p31._0(state));
			});
	});
var _evancz$url_parser$UrlParser$QueryParser = function (a) {
	return {ctor: 'QueryParser', _0: a};
};
var _evancz$url_parser$UrlParser$customParam = F2(
	function (key, func) {
		return _evancz$url_parser$UrlParser$QueryParser(
			function (_p33) {
				var _p34 = _p33;
				var _p35 = _p34.params;
				return {
					ctor: '::',
					_0: A4(
						_evancz$url_parser$UrlParser$State,
						_p34.visited,
						_p34.unvisited,
						_p35,
						_p34.value(
							func(
								A2(_elm_lang$core$Dict$get, key, _p35)))),
					_1: {ctor: '[]'}
				};
			});
	});
var _evancz$url_parser$UrlParser$stringParam = function (name) {
	return A2(_evancz$url_parser$UrlParser$customParam, name, _elm_lang$core$Basics$identity);
};
var _evancz$url_parser$UrlParser$intParam = function (name) {
	return A2(_evancz$url_parser$UrlParser$customParam, name, _evancz$url_parser$UrlParser$intParamHelp);
};

var _fredcy$elm_parseint$ParseInt$charFromInt = function (i) {
	return (_elm_lang$core$Native_Utils.cmp(i, 10) < 0) ? _elm_lang$core$Char$fromCode(
		i + _elm_lang$core$Char$toCode(
			_elm_lang$core$Native_Utils.chr('0'))) : ((_elm_lang$core$Native_Utils.cmp(i, 36) < 0) ? _elm_lang$core$Char$fromCode(
		(i - 10) + _elm_lang$core$Char$toCode(
			_elm_lang$core$Native_Utils.chr('A'))) : _elm_lang$core$Native_Utils.crash(
		'ParseInt',
		{
			start: {line: 158, column: 9},
			end: {line: 158, column: 20}
		})(
		_elm_lang$core$Basics$toString(i)));
};
var _fredcy$elm_parseint$ParseInt$toRadixUnsafe = F2(
	function (radix, i) {
		return (_elm_lang$core$Native_Utils.cmp(i, radix) < 0) ? _elm_lang$core$String$fromChar(
			_fredcy$elm_parseint$ParseInt$charFromInt(i)) : A2(
			_elm_lang$core$Basics_ops['++'],
			A2(_fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, (i / radix) | 0),
			_elm_lang$core$String$fromChar(
				_fredcy$elm_parseint$ParseInt$charFromInt(
					A2(_elm_lang$core$Basics_ops['%'], i, radix))));
	});
var _fredcy$elm_parseint$ParseInt$toOct = _fredcy$elm_parseint$ParseInt$toRadixUnsafe(8);
var _fredcy$elm_parseint$ParseInt$toHex = _fredcy$elm_parseint$ParseInt$toRadixUnsafe(16);
var _fredcy$elm_parseint$ParseInt$isBetween = F3(
	function (lower, upper, c) {
		var ci = _elm_lang$core$Char$toCode(c);
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$Char$toCode(lower),
			ci) < 1) && (_elm_lang$core$Native_Utils.cmp(
			ci,
			_elm_lang$core$Char$toCode(upper)) < 1);
	});
var _fredcy$elm_parseint$ParseInt$charOffset = F2(
	function (basis, c) {
		return _elm_lang$core$Char$toCode(c) - _elm_lang$core$Char$toCode(basis);
	});
var _fredcy$elm_parseint$ParseInt$InvalidRadix = function (a) {
	return {ctor: 'InvalidRadix', _0: a};
};
var _fredcy$elm_parseint$ParseInt$toRadix = F2(
	function (radix, i) {
		return ((_elm_lang$core$Native_Utils.cmp(2, radix) < 1) && (_elm_lang$core$Native_Utils.cmp(radix, 36) < 1)) ? ((_elm_lang$core$Native_Utils.cmp(i, 0) < 0) ? _elm_lang$core$Result$Ok(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'-',
				A2(_fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, 0 - i))) : _elm_lang$core$Result$Ok(
			A2(_fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, i))) : _elm_lang$core$Result$Err(
			_fredcy$elm_parseint$ParseInt$InvalidRadix(radix));
	});
var _fredcy$elm_parseint$ParseInt$OutOfRange = function (a) {
	return {ctor: 'OutOfRange', _0: a};
};
var _fredcy$elm_parseint$ParseInt$InvalidChar = function (a) {
	return {ctor: 'InvalidChar', _0: a};
};
var _fredcy$elm_parseint$ParseInt$intFromChar = F2(
	function (radix, c) {
		var validInt = function (i) {
			return (_elm_lang$core$Native_Utils.cmp(i, radix) < 0) ? _elm_lang$core$Result$Ok(i) : _elm_lang$core$Result$Err(
				_fredcy$elm_parseint$ParseInt$OutOfRange(c));
		};
		var toInt = A3(
			_fredcy$elm_parseint$ParseInt$isBetween,
			_elm_lang$core$Native_Utils.chr('0'),
			_elm_lang$core$Native_Utils.chr('9'),
			c) ? _elm_lang$core$Result$Ok(
			A2(
				_fredcy$elm_parseint$ParseInt$charOffset,
				_elm_lang$core$Native_Utils.chr('0'),
				c)) : (A3(
			_fredcy$elm_parseint$ParseInt$isBetween,
			_elm_lang$core$Native_Utils.chr('a'),
			_elm_lang$core$Native_Utils.chr('z'),
			c) ? _elm_lang$core$Result$Ok(
			10 + A2(
				_fredcy$elm_parseint$ParseInt$charOffset,
				_elm_lang$core$Native_Utils.chr('a'),
				c)) : (A3(
			_fredcy$elm_parseint$ParseInt$isBetween,
			_elm_lang$core$Native_Utils.chr('A'),
			_elm_lang$core$Native_Utils.chr('Z'),
			c) ? _elm_lang$core$Result$Ok(
			10 + A2(
				_fredcy$elm_parseint$ParseInt$charOffset,
				_elm_lang$core$Native_Utils.chr('A'),
				c)) : _elm_lang$core$Result$Err(
			_fredcy$elm_parseint$ParseInt$InvalidChar(c))));
		return A2(_elm_lang$core$Result$andThen, validInt, toInt);
	});
var _fredcy$elm_parseint$ParseInt$parseIntR = F2(
	function (radix, rstring) {
		var _p0 = _elm_lang$core$String$uncons(rstring);
		if (_p0.ctor === 'Nothing') {
			return _elm_lang$core$Result$Ok(0);
		} else {
			return A2(
				_elm_lang$core$Result$andThen,
				function (ci) {
					return A2(
						_elm_lang$core$Result$andThen,
						function (ri) {
							return _elm_lang$core$Result$Ok(ci + (ri * radix));
						},
						A2(_fredcy$elm_parseint$ParseInt$parseIntR, radix, _p0._0._1));
				},
				A2(_fredcy$elm_parseint$ParseInt$intFromChar, radix, _p0._0._0));
		}
	});
var _fredcy$elm_parseint$ParseInt$parseIntRadix = F2(
	function (radix, string) {
		return ((_elm_lang$core$Native_Utils.cmp(2, radix) < 1) && (_elm_lang$core$Native_Utils.cmp(radix, 36) < 1)) ? A2(
			_fredcy$elm_parseint$ParseInt$parseIntR,
			radix,
			_elm_lang$core$String$reverse(string)) : _elm_lang$core$Result$Err(
			_fredcy$elm_parseint$ParseInt$InvalidRadix(radix));
	});
var _fredcy$elm_parseint$ParseInt$parseInt = _fredcy$elm_parseint$ParseInt$parseIntRadix(10);
var _fredcy$elm_parseint$ParseInt$parseIntOct = _fredcy$elm_parseint$ParseInt$parseIntRadix(8);
var _fredcy$elm_parseint$ParseInt$parseIntHex = _fredcy$elm_parseint$ParseInt$parseIntRadix(16);

var _krisajenkins$remotedata$RemoteData$isNotAsked = function (data) {
	var _p0 = data;
	if (_p0.ctor === 'NotAsked') {
		return true;
	} else {
		return false;
	}
};
var _krisajenkins$remotedata$RemoteData$isLoading = function (data) {
	var _p1 = data;
	if (_p1.ctor === 'Loading') {
		return true;
	} else {
		return false;
	}
};
var _krisajenkins$remotedata$RemoteData$isFailure = function (data) {
	var _p2 = data;
	if (_p2.ctor === 'Failure') {
		return true;
	} else {
		return false;
	}
};
var _krisajenkins$remotedata$RemoteData$isSuccess = function (data) {
	var _p3 = data;
	if (_p3.ctor === 'Success') {
		return true;
	} else {
		return false;
	}
};
var _krisajenkins$remotedata$RemoteData$withDefault = F2(
	function ($default, data) {
		var _p4 = data;
		if (_p4.ctor === 'Success') {
			return _p4._0;
		} else {
			return $default;
		}
	});
var _krisajenkins$remotedata$RemoteData$Success = function (a) {
	return {ctor: 'Success', _0: a};
};
var _krisajenkins$remotedata$RemoteData$succeed = _krisajenkins$remotedata$RemoteData$Success;
var _krisajenkins$remotedata$RemoteData$prism = {
	reverseGet: _krisajenkins$remotedata$RemoteData$Success,
	getOption: function (data) {
		var _p5 = data;
		if (_p5.ctor === 'Success') {
			return _elm_lang$core$Maybe$Just(_p5._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	}
};
var _krisajenkins$remotedata$RemoteData$Failure = function (a) {
	return {ctor: 'Failure', _0: a};
};
var _krisajenkins$remotedata$RemoteData$fromResult = function (result) {
	var _p6 = result;
	if (_p6.ctor === 'Err') {
		return _krisajenkins$remotedata$RemoteData$Failure(_p6._0);
	} else {
		return _krisajenkins$remotedata$RemoteData$Success(_p6._0);
	}
};
var _krisajenkins$remotedata$RemoteData$asCmd = _elm_lang$core$Task$attempt(_krisajenkins$remotedata$RemoteData$fromResult);
var _krisajenkins$remotedata$RemoteData$sendRequest = _elm_lang$http$Http$send(_krisajenkins$remotedata$RemoteData$fromResult);
var _krisajenkins$remotedata$RemoteData$fromTask = function (_p7) {
	return A2(
		_elm_lang$core$Task$onError,
		function (_p8) {
			return _elm_lang$core$Task$succeed(
				_krisajenkins$remotedata$RemoteData$Failure(_p8));
		},
		A2(_elm_lang$core$Task$map, _krisajenkins$remotedata$RemoteData$Success, _p7));
};
var _krisajenkins$remotedata$RemoteData$Loading = {ctor: 'Loading'};
var _krisajenkins$remotedata$RemoteData$NotAsked = {ctor: 'NotAsked'};
var _krisajenkins$remotedata$RemoteData$map = F2(
	function (f, data) {
		var _p9 = data;
		switch (_p9.ctor) {
			case 'Success':
				return _krisajenkins$remotedata$RemoteData$Success(
					f(_p9._0));
			case 'Loading':
				return _krisajenkins$remotedata$RemoteData$Loading;
			case 'NotAsked':
				return _krisajenkins$remotedata$RemoteData$NotAsked;
			default:
				return _krisajenkins$remotedata$RemoteData$Failure(_p9._0);
		}
	});
var _krisajenkins$remotedata$RemoteData$toMaybe = function (_p10) {
	return A2(
		_krisajenkins$remotedata$RemoteData$withDefault,
		_elm_lang$core$Maybe$Nothing,
		A2(_krisajenkins$remotedata$RemoteData$map, _elm_lang$core$Maybe$Just, _p10));
};
var _krisajenkins$remotedata$RemoteData$mapError = F2(
	function (f, data) {
		var _p11 = data;
		switch (_p11.ctor) {
			case 'Success':
				return _krisajenkins$remotedata$RemoteData$Success(_p11._0);
			case 'Failure':
				return _krisajenkins$remotedata$RemoteData$Failure(
					f(_p11._0));
			case 'Loading':
				return _krisajenkins$remotedata$RemoteData$Loading;
			default:
				return _krisajenkins$remotedata$RemoteData$NotAsked;
		}
	});
var _krisajenkins$remotedata$RemoteData$mapBoth = F3(
	function (successFn, errorFn, data) {
		var _p12 = data;
		switch (_p12.ctor) {
			case 'Success':
				return _krisajenkins$remotedata$RemoteData$Success(
					successFn(_p12._0));
			case 'Failure':
				return _krisajenkins$remotedata$RemoteData$Failure(
					errorFn(_p12._0));
			case 'Loading':
				return _krisajenkins$remotedata$RemoteData$Loading;
			default:
				return _krisajenkins$remotedata$RemoteData$NotAsked;
		}
	});
var _krisajenkins$remotedata$RemoteData$andThen = F2(
	function (f, data) {
		var _p13 = data;
		switch (_p13.ctor) {
			case 'Success':
				return f(_p13._0);
			case 'Failure':
				return _krisajenkins$remotedata$RemoteData$Failure(_p13._0);
			case 'NotAsked':
				return _krisajenkins$remotedata$RemoteData$NotAsked;
			default:
				return _krisajenkins$remotedata$RemoteData$Loading;
		}
	});
var _krisajenkins$remotedata$RemoteData$andMap = F2(
	function (wrappedValue, wrappedFunction) {
		var _p14 = wrappedFunction;
		switch (_p14.ctor) {
			case 'Success':
				return A2(_krisajenkins$remotedata$RemoteData$map, _p14._0, wrappedValue);
			case 'Failure':
				return _krisajenkins$remotedata$RemoteData$Failure(_p14._0);
			case 'Loading':
				return _krisajenkins$remotedata$RemoteData$Loading;
			default:
				return _krisajenkins$remotedata$RemoteData$NotAsked;
		}
	});
var _krisajenkins$remotedata$RemoteData$map2 = F3(
	function (f, a, b) {
		return A2(
			_krisajenkins$remotedata$RemoteData$andMap,
			b,
			A2(_krisajenkins$remotedata$RemoteData$map, f, a));
	});
var _krisajenkins$remotedata$RemoteData$map3 = F4(
	function (f, a, b, c) {
		return A2(
			_krisajenkins$remotedata$RemoteData$andMap,
			c,
			A2(
				_krisajenkins$remotedata$RemoteData$andMap,
				b,
				A2(_krisajenkins$remotedata$RemoteData$map, f, a)));
	});
var _krisajenkins$remotedata$RemoteData$append = F2(
	function (a, b) {
		return A2(
			_krisajenkins$remotedata$RemoteData$andMap,
			b,
			A2(
				_krisajenkins$remotedata$RemoteData$map,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					}),
				a));
	});
var _krisajenkins$remotedata$RemoteData$update = F2(
	function (f, remoteData) {
		var _p15 = remoteData;
		switch (_p15.ctor) {
			case 'Success':
				var _p16 = f(_p15._0);
				var first = _p16._0;
				var second = _p16._1;
				return {
					ctor: '_Tuple2',
					_0: _krisajenkins$remotedata$RemoteData$Success(first),
					_1: second
				};
			case 'NotAsked':
				return {ctor: '_Tuple2', _0: _krisajenkins$remotedata$RemoteData$NotAsked, _1: _elm_lang$core$Platform_Cmd$none};
			case 'Loading':
				return {ctor: '_Tuple2', _0: _krisajenkins$remotedata$RemoteData$Loading, _1: _elm_lang$core$Platform_Cmd$none};
			default:
				return {
					ctor: '_Tuple2',
					_0: _krisajenkins$remotedata$RemoteData$Failure(_p15._0),
					_1: _elm_lang$core$Platform_Cmd$none
				};
		}
	});

var _rluiten$elm_date_extra$Date_Extra_Internal2$prevMonth = function (month) {
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
var _rluiten$elm_date_extra$Date_Extra_Internal2$nextMonth = function (month) {
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
var _rluiten$elm_date_extra$Date_Extra_Internal2$intToMonth = function (month) {
	return (_elm_lang$core$Native_Utils.cmp(month, 1) < 1) ? _elm_lang$core$Date$Jan : (_elm_lang$core$Native_Utils.eq(month, 2) ? _elm_lang$core$Date$Feb : (_elm_lang$core$Native_Utils.eq(month, 3) ? _elm_lang$core$Date$Mar : (_elm_lang$core$Native_Utils.eq(month, 4) ? _elm_lang$core$Date$Apr : (_elm_lang$core$Native_Utils.eq(month, 5) ? _elm_lang$core$Date$May : (_elm_lang$core$Native_Utils.eq(month, 6) ? _elm_lang$core$Date$Jun : (_elm_lang$core$Native_Utils.eq(month, 7) ? _elm_lang$core$Date$Jul : (_elm_lang$core$Native_Utils.eq(month, 8) ? _elm_lang$core$Date$Aug : (_elm_lang$core$Native_Utils.eq(month, 9) ? _elm_lang$core$Date$Sep : (_elm_lang$core$Native_Utils.eq(month, 10) ? _elm_lang$core$Date$Oct : (_elm_lang$core$Native_Utils.eq(month, 11) ? _elm_lang$core$Date$Nov : _elm_lang$core$Date$Dec))))))))));
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$monthToInt = function (month) {
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
var _rluiten$elm_date_extra$Date_Extra_Internal2$isLeapYear = function (year) {
	return (_elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], year, 4),
		0) && (!_elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], year, 100),
		0))) || _elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], year, 400),
		0);
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$isLeapYearDate = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Internal2$isLeapYear(
		_elm_lang$core$Date$year(date));
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$yearToDayLength = function (year) {
	return _rluiten$elm_date_extra$Date_Extra_Internal2$isLeapYear(year) ? 366 : 365;
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$daysInMonth = F2(
	function (year, month) {
		var _p3 = month;
		switch (_p3.ctor) {
			case 'Jan':
				return 31;
			case 'Feb':
				return _rluiten$elm_date_extra$Date_Extra_Internal2$isLeapYear(year) ? 29 : 28;
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
var _rluiten$elm_date_extra$Date_Extra_Internal2$daysInMonthDate = function (date) {
	return A2(
		_rluiten$elm_date_extra$Date_Extra_Internal2$daysInMonth,
		_elm_lang$core$Date$year(date),
		_elm_lang$core$Date$month(date));
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$monthList = {
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
var _rluiten$elm_date_extra$Date_Extra_Internal2$toTime = function (_p4) {
	return _elm_lang$core$Basics$floor(
		_elm_lang$core$Date$toTime(_p4));
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$fromTime = function (_p5) {
	return _elm_lang$core$Date$fromTime(
		_elm_lang$core$Basics$toFloat(_p5));
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$prevDay = function (day) {
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
var _rluiten$elm_date_extra$Date_Extra_Internal2$nextDay = function (day) {
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
var _rluiten$elm_date_extra$Date_Extra_Internal2$isoDayOfWeek = function (day) {
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
var _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMillisecond = _elm_lang$core$Basics$floor(_elm_lang$core$Time$millisecond);
var _rluiten$elm_date_extra$Date_Extra_Internal2$ticksASecond = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMillisecond * 1000;
var _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMinute = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksASecond * 60;
var _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAnHour = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMinute * 60;
var _rluiten$elm_date_extra$Date_Extra_Internal2$ticksADay = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAnHour * 24;
var _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAWeek = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksADay * 7;
var _rluiten$elm_date_extra$Date_Extra_Internal2$lastOfMonthTicks = function (date) {
	var dateTicks = _rluiten$elm_date_extra$Date_Extra_Internal2$toTime(date);
	var day = _elm_lang$core$Date$day(date);
	var month = _elm_lang$core$Date$month(date);
	var year = _elm_lang$core$Date$year(date);
	var daysInMonthVal = A2(_rluiten$elm_date_extra$Date_Extra_Internal2$daysInMonth, year, month);
	var addDays = daysInMonthVal - day;
	return dateTicks + (addDays * _rluiten$elm_date_extra$Date_Extra_Internal2$ticksADay);
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$firstOfNextMonthDate = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Internal2$fromTime(
		_rluiten$elm_date_extra$Date_Extra_Internal2$lastOfMonthTicks(date) + _rluiten$elm_date_extra$Date_Extra_Internal2$ticksADay);
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$daysInNextMonth = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Internal2$daysInMonthDate(
		_rluiten$elm_date_extra$Date_Extra_Internal2$firstOfNextMonthDate(date));
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$firstOfMonthTicks = function (date) {
	var dateTicks = _rluiten$elm_date_extra$Date_Extra_Internal2$toTime(date);
	var day = _elm_lang$core$Date$day(date);
	return dateTicks + ((1 - day) * _rluiten$elm_date_extra$Date_Extra_Internal2$ticksADay);
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$lastOfPrevMonthDate = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Internal2$fromTime(
		_rluiten$elm_date_extra$Date_Extra_Internal2$firstOfMonthTicks(date) - _rluiten$elm_date_extra$Date_Extra_Internal2$ticksADay);
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$daysInPrevMonth = function (date) {
	return _rluiten$elm_date_extra$Date_Extra_Internal2$daysInMonthDate(
		_rluiten$elm_date_extra$Date_Extra_Internal2$lastOfPrevMonthDate(date));
};
var _rluiten$elm_date_extra$Date_Extra_Internal2$epochDateStr = '1970-01-01T00:00:00Z';

var _rluiten$elm_date_extra$Date_Extra_Period$diff = F2(
	function (date1, date2) {
		var millisecondDiff = _elm_lang$core$Date$millisecond(date1) - _elm_lang$core$Date$millisecond(date2);
		var secondDiff = _elm_lang$core$Date$second(date1) - _elm_lang$core$Date$second(date2);
		var minuteDiff = _elm_lang$core$Date$minute(date1) - _elm_lang$core$Date$minute(date2);
		var hourDiff = _elm_lang$core$Date$hour(date1) - _elm_lang$core$Date$hour(date2);
		var ticksDiff = _rluiten$elm_date_extra$Date_Extra_Internal2$toTime(date1) - _rluiten$elm_date_extra$Date_Extra_Internal2$toTime(date2);
		var ticksDayDiff = (((ticksDiff - (hourDiff * _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAnHour)) - (minuteDiff * _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMinute)) - (secondDiff * _rluiten$elm_date_extra$Date_Extra_Internal2$ticksASecond)) - (millisecondDiff * _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMillisecond);
		var onlyDaysDiff = (ticksDayDiff / _rluiten$elm_date_extra$Date_Extra_Internal2$ticksADay) | 0;
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
		return _rluiten$elm_date_extra$Date_Extra_Internal2$fromTime(
			A2(
				F2(
					function (x, y) {
						return x + y;
					}),
				addend * unit,
				_rluiten$elm_date_extra$Date_Extra_Internal2$toTime(date)));
	});
var _rluiten$elm_date_extra$Date_Extra_Period$toTicks = function (period) {
	var _p1 = period;
	switch (_p1.ctor) {
		case 'Millisecond':
			return _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMillisecond;
		case 'Second':
			return _rluiten$elm_date_extra$Date_Extra_Internal2$ticksASecond;
		case 'Minute':
			return _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMinute;
		case 'Hour':
			return _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAnHour;
		case 'Day':
			return _rluiten$elm_date_extra$Date_Extra_Internal2$ticksADay;
		case 'Week':
			return _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAWeek;
		default:
			var _p2 = _p1._0;
			return (((((_rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMillisecond * _p2.millisecond) + (_rluiten$elm_date_extra$Date_Extra_Internal2$ticksASecond * _p2.second)) + (_rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMinute * _p2.minute)) + (_rluiten$elm_date_extra$Date_Extra_Internal2$ticksAnHour * _p2.hour)) + (_rluiten$elm_date_extra$Date_Extra_Internal2$ticksADay * _p2.day)) + (_rluiten$elm_date_extra$Date_Extra_Internal2$ticksAWeek * _p2.week);
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
		var monthInt = _rluiten$elm_date_extra$Date_Extra_Internal2$monthToInt(month);
		var clampYear = (_elm_lang$core$Native_Utils.cmp(year, 0) < 0) ? 0 : year;
		var clampDay = A3(
			_elm_lang$core$Basics$clamp,
			1,
			A2(_rluiten$elm_date_extra$Date_Extra_Internal2$daysInMonth, clampYear, month),
			day);
		var dayCount = A3(_rluiten$elm_date_extra$Date_Extra_Internal$daysFromCivil, clampYear, monthInt, clampDay);
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
	return ((dateTicks - v1Ticks) / _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMinute) | 0;
};
var _rluiten$elm_date_extra$Date_Extra_Internal$hackDateAsOffset = F2(
	function (offsetMinutes, date) {
		return _rluiten$elm_date_extra$Date_Extra_Internal2$fromTime(
			A2(
				F2(
					function (x, y) {
						return x + y;
					}),
				offsetMinutes * _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMinute,
				_rluiten$elm_date_extra$Date_Extra_Internal2$toTime(date)));
	});
var _rluiten$elm_date_extra$Date_Extra_Internal$hackDateAsUtc = function (date) {
	var offset = _rluiten$elm_date_extra$Date_Extra_Internal$getTimezoneOffset(date);
	var oHours = (offset / _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAnHour) | 0;
	var oMinutes = ((offset - (oHours * _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAnHour)) / _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMinute) | 0;
	return A2(_rluiten$elm_date_extra$Date_Extra_Internal$hackDateAsOffset, offset, date);
};

var _rluiten$elm_date_extra$Date_Extra_Core$compensateZoneOffset = F2(
	function (date1, date2) {
		return A3(
			_rluiten$elm_date_extra$Date_Extra_Period$add,
			_rluiten$elm_date_extra$Date_Extra_Period$Minute,
			_rluiten$elm_date_extra$Date_Extra_Internal$getTimezoneOffset(date2) - _rluiten$elm_date_extra$Date_Extra_Internal$getTimezoneOffset(date1),
			date2);
	});
var _rluiten$elm_date_extra$Date_Extra_Core$lastOfMonthDate = function (date) {
	return A2(
		_rluiten$elm_date_extra$Date_Extra_Core$compensateZoneOffset,
		date,
		_rluiten$elm_date_extra$Date_Extra_Internal2$fromTime(
			_rluiten$elm_date_extra$Date_Extra_Internal2$lastOfMonthTicks(date)));
};
var _rluiten$elm_date_extra$Date_Extra_Core$toFirstOfMonth = function (date) {
	return A2(
		_rluiten$elm_date_extra$Date_Extra_Core$compensateZoneOffset,
		date,
		_rluiten$elm_date_extra$Date_Extra_Internal2$fromTime(
			_rluiten$elm_date_extra$Date_Extra_Internal2$firstOfMonthTicks(date)));
};
var _rluiten$elm_date_extra$Date_Extra_Core$lastOfPrevMonthDate = function (date) {
	return A2(
		_rluiten$elm_date_extra$Date_Extra_Core$compensateZoneOffset,
		date,
		_rluiten$elm_date_extra$Date_Extra_Internal2$lastOfPrevMonthDate(date));
};
var _rluiten$elm_date_extra$Date_Extra_Core$firstOfNextMonthDate = function (date) {
	return A2(
		_rluiten$elm_date_extra$Date_Extra_Core$compensateZoneOffset,
		date,
		_rluiten$elm_date_extra$Date_Extra_Internal2$firstOfNextMonthDate(date));
};
var _rluiten$elm_date_extra$Date_Extra_Core$yearToDayLength = _rluiten$elm_date_extra$Date_Extra_Internal2$yearToDayLength;
var _rluiten$elm_date_extra$Date_Extra_Core$toTime = _rluiten$elm_date_extra$Date_Extra_Internal2$toTime;
var _rluiten$elm_date_extra$Date_Extra_Core$ticksAWeek = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAWeek;
var _rluiten$elm_date_extra$Date_Extra_Core$ticksASecond = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksASecond;
var _rluiten$elm_date_extra$Date_Extra_Core$ticksAMinute = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMinute;
var _rluiten$elm_date_extra$Date_Extra_Core$ticksAMillisecond = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAMillisecond;
var _rluiten$elm_date_extra$Date_Extra_Core$ticksADay = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksADay;
var _rluiten$elm_date_extra$Date_Extra_Core$ticksAnHour = _rluiten$elm_date_extra$Date_Extra_Internal2$ticksAnHour;
var _rluiten$elm_date_extra$Date_Extra_Core$prevMonth = _rluiten$elm_date_extra$Date_Extra_Internal2$prevMonth;
var _rluiten$elm_date_extra$Date_Extra_Core$prevDay = _rluiten$elm_date_extra$Date_Extra_Internal2$prevDay;
var _rluiten$elm_date_extra$Date_Extra_Core$nextMonth = _rluiten$elm_date_extra$Date_Extra_Internal2$nextMonth;
var _rluiten$elm_date_extra$Date_Extra_Core$nextDay = _rluiten$elm_date_extra$Date_Extra_Internal2$nextDay;
var _rluiten$elm_date_extra$Date_Extra_Core$monthToInt = _rluiten$elm_date_extra$Date_Extra_Internal2$monthToInt;
var _rluiten$elm_date_extra$Date_Extra_Core$monthList = _rluiten$elm_date_extra$Date_Extra_Internal2$monthList;
var _rluiten$elm_date_extra$Date_Extra_Core$isoDayOfWeek = _rluiten$elm_date_extra$Date_Extra_Internal2$isoDayOfWeek;
var _rluiten$elm_date_extra$Date_Extra_Core$daysBackToStartOfWeek = F2(
	function (dateDay, startOfWeekDay) {
		var startOfWeekDayIndex = _rluiten$elm_date_extra$Date_Extra_Core$isoDayOfWeek(startOfWeekDay);
		var dateDayIndex = _rluiten$elm_date_extra$Date_Extra_Core$isoDayOfWeek(dateDay);
		return (_elm_lang$core$Native_Utils.cmp(dateDayIndex, startOfWeekDayIndex) < 0) ? ((7 + dateDayIndex) - startOfWeekDayIndex) : (dateDayIndex - startOfWeekDayIndex);
	});
var _rluiten$elm_date_extra$Date_Extra_Core$isLeapYearDate = _rluiten$elm_date_extra$Date_Extra_Internal2$isLeapYearDate;
var _rluiten$elm_date_extra$Date_Extra_Core$isLeapYear = _rluiten$elm_date_extra$Date_Extra_Internal2$isLeapYear;
var _rluiten$elm_date_extra$Date_Extra_Core$intToMonth = _rluiten$elm_date_extra$Date_Extra_Internal2$intToMonth;
var _rluiten$elm_date_extra$Date_Extra_Core$fromTime = _rluiten$elm_date_extra$Date_Extra_Internal2$fromTime;
var _rluiten$elm_date_extra$Date_Extra_Core$epochDateStr = _rluiten$elm_date_extra$Date_Extra_Internal2$epochDateStr;
var _rluiten$elm_date_extra$Date_Extra_Core$daysInMonthDate = _rluiten$elm_date_extra$Date_Extra_Internal2$daysInMonthDate;
var _rluiten$elm_date_extra$Date_Extra_Core$daysInPrevMonth = _rluiten$elm_date_extra$Date_Extra_Internal2$daysInPrevMonth;
var _rluiten$elm_date_extra$Date_Extra_Core$daysInNextMonth = _rluiten$elm_date_extra$Date_Extra_Internal2$daysInNextMonth;
var _rluiten$elm_date_extra$Date_Extra_Core$daysInMonth = _rluiten$elm_date_extra$Date_Extra_Internal2$daysInMonth;

var _rluiten$elm_date_extra$Date_Extra_Config$Config = F2(
	function (a, b) {
		return {i18n: a, format: b};
	});

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

var _user$project$Data_Michelson$Script = F2(
	function (a, b) {
		return {code: a, storage: b};
	});
var _user$project$Data_Michelson$Code = F4(
	function (a, b, c, d) {
		return {code: a, argType: b, retType: c, storageType: d};
	});
var _user$project$Data_Michelson$Storage = F2(
	function (a, b) {
		return {storage: a, storageType: b};
	});
var _user$project$Data_Michelson$decodeStorage = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'storageType',
	_elm_lang$core$Json_Decode$value,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'storage',
		_elm_lang$core$Json_Decode$value,
		_elm_lang$core$Json_Decode$succeed(_user$project$Data_Michelson$Storage)));
var _user$project$Data_Michelson$PrimArgT = F2(
	function (a, b) {
		return {ctor: 'PrimArgT', _0: a, _1: b};
	});
var _user$project$Data_Michelson$PrimT = function (a) {
	return {ctor: 'PrimT', _0: a};
};
var _user$project$Data_Michelson$decodePrimT = A2(_elm_lang$core$Json_Decode$map, _user$project$Data_Michelson$PrimT, _elm_lang$core$Json_Decode$string);
var _user$project$Data_Michelson$SeqT = function (a) {
	return {ctor: 'SeqT', _0: a};
};
var _user$project$Data_Michelson$StringT = function (a) {
	return {ctor: 'StringT', _0: a};
};
var _user$project$Data_Michelson$decodeStringT = A2(
	_elm_lang$core$Json_Decode$map,
	_user$project$Data_Michelson$StringT,
	A2(_elm_lang$core$Json_Decode$field, 'string', _elm_lang$core$Json_Decode$string));
var _user$project$Data_Michelson$IntT = function (a) {
	return {ctor: 'IntT', _0: a};
};
var _user$project$Data_Michelson$decodeIntT = A2(
	_elm_lang$core$Json_Decode$andThen,
	function (result) {
		var _p0 = result;
		if (_p0.ctor === 'Ok') {
			return _elm_lang$core$Json_Decode$succeed(
				_user$project$Data_Michelson$IntT(_p0._0));
		} else {
			return _elm_lang$core$Json_Decode$fail('bad int');
		}
	},
	A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$String$toInt,
		A2(_elm_lang$core$Json_Decode$field, 'int', _elm_lang$core$Json_Decode$string)));
var _user$project$Data_Michelson$decodeAST = _elm_lang$core$Json_Decode$lazy(
	function (_p1) {
		return _elm_lang$core$Json_Decode$oneOf(
			{
				ctor: '::',
				_0: _user$project$Data_Michelson$decodeIntT,
				_1: {
					ctor: '::',
					_0: _user$project$Data_Michelson$decodeStringT,
					_1: {
						ctor: '::',
						_0: _user$project$Data_Michelson$decodeSeqT,
						_1: {
							ctor: '::',
							_0: _user$project$Data_Michelson$decodePrimT,
							_1: {
								ctor: '::',
								_0: _user$project$Data_Michelson$decodePrimArgT,
								_1: {ctor: '[]'}
							}
						}
					}
				}
			});
	});
var _user$project$Data_Michelson$decodePrimArgT = A2(
	_elm_lang$core$Json_Decode$andThen,
	function (kvPairs) {
		var _p2 = kvPairs;
		if (((_p2.ctor === '::') && (_p2._0.ctor === '_Tuple2')) && (_p2._1.ctor === '[]')) {
			return _elm_lang$core$Json_Decode$succeed(
				A2(_user$project$Data_Michelson$PrimArgT, _p2._0._0, _p2._0._1));
		} else {
			return _elm_lang$core$Json_Decode$fail('bad kvPairs');
		}
	},
	_elm_lang$core$Json_Decode$keyValuePairs(
		_elm_lang$core$Json_Decode$lazy(
			function (_p3) {
				return _user$project$Data_Michelson$decodeAST;
			})));
var _user$project$Data_Michelson$decodeSeqT = A2(
	_elm_lang$core$Json_Decode$map,
	_user$project$Data_Michelson$SeqT,
	_elm_lang$core$Json_Decode$list(
		_elm_lang$core$Json_Decode$lazy(
			function (_p4) {
				return _user$project$Data_Michelson$decodeAST;
			})));
var _user$project$Data_Michelson$decodeProgram = _user$project$Data_Michelson$decodeSeqT;
var _user$project$Data_Michelson$decodeCode = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'storageType',
	_elm_lang$core$Json_Decode$value,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'retType',
		_elm_lang$core$Json_Decode$value,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'argType',
			_elm_lang$core$Json_Decode$value,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'code',
				_user$project$Data_Michelson$decodeProgram,
				_elm_lang$core$Json_Decode$succeed(_user$project$Data_Michelson$Code)))));
var _user$project$Data_Michelson$decodeScript = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'storage',
	_user$project$Data_Michelson$decodeStorage,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'code',
		_user$project$Data_Michelson$decodeCode,
		_elm_lang$core$Json_Decode$succeed(_user$project$Data_Michelson$Script)));

var _user$project$Data_Chain$decodeStringToInt = A2(
	_elm_lang$core$Json_Decode$andThen,
	function (intString) {
		var _p0 = _elm_lang$core$String$toInt(intString);
		if (_p0.ctor === 'Ok') {
			return _elm_lang$core$Json_Decode$succeed(_p0._0);
		} else {
			return _elm_lang$core$Json_Decode$fail(_p0._0);
		}
	},
	_elm_lang$core$Json_Decode$string);
var _user$project$Data_Chain$decodeBalance = _elm_lang$core$Json_Decode$oneOf(
	{
		ctor: '::',
		_0: _elm_lang$core$Json_Decode$int,
		_1: {
			ctor: '::',
			_0: _user$project$Data_Chain$decodeStringToInt,
			_1: {ctor: '[]'}
		}
	});
var _user$project$Data_Chain$decodeContractIDs = A2(
	_elm_lang$core$Json_Decode$field,
	'ok',
	_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string));
var _user$project$Data_Chain$decodeLevel = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'ok',
		_1: {
			ctor: '::',
			_0: 'level',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$int);
var _user$project$Data_Chain$decodeTimestamp = A2(
	_elm_lang$core$Json_Decode$andThen,
	function (dateResult) {
		var _p1 = dateResult;
		if (_p1.ctor === 'Ok') {
			return _elm_lang$core$Json_Decode$succeed(_p1._0);
		} else {
			return _elm_lang$core$Json_Decode$fail(_p1._0);
		}
	},
	A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Date$fromString, _elm_lang$core$Json_Decode$string));
var _user$project$Data_Chain$decodeHexString = A2(
	_elm_lang$core$Json_Decode$andThen,
	function (hexString) {
		var _p2 = _fredcy$elm_parseint$ParseInt$parseIntHex(hexString);
		if (_p2.ctor === 'Ok') {
			return _elm_lang$core$Json_Decode$succeed(_p2._0);
		} else {
			return _elm_lang$core$Json_Decode$fail(
				_elm_lang$core$Basics$toString(_p2._0));
		}
	},
	_elm_lang$core$Json_Decode$string);
var _user$project$Data_Chain$decodeFitness = _elm_lang$core$Json_Decode$list(_user$project$Data_Chain$decodeHexString);
var _user$project$Data_Chain$loadContractError = F3(
	function (model, contractId, error) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				contracts: A3(
					_elm_lang$core$Dict$insert,
					contractId,
					_krisajenkins$remotedata$RemoteData$Failure(error),
					model.contracts)
			});
	});
var _user$project$Data_Chain$loadContract = F3(
	function (model, contractId, contract) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				contracts: A3(
					_elm_lang$core$Dict$insert,
					contractId,
					_krisajenkins$remotedata$RemoteData$Success(contract),
					model.contracts)
			});
	});
var _user$project$Data_Chain$loadingContract = F2(
	function (model, contractId) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				contracts: A3(_elm_lang$core$Dict$insert, contractId, _krisajenkins$remotedata$RemoteData$Loading, model.contracts)
			});
	});
var _user$project$Data_Chain$loadPeersError = F2(
	function (model, error) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				peers: _krisajenkins$remotedata$RemoteData$Failure(error)
			});
	});
var _user$project$Data_Chain$loadPeers = F2(
	function (model, peers) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				peers: _krisajenkins$remotedata$RemoteData$Success(peers)
			});
	});
var _user$project$Data_Chain$loadingPeers = function (model) {
	return _elm_lang$core$Native_Utils.update(
		model,
		{peers: _krisajenkins$remotedata$RemoteData$Loading});
};
var _user$project$Data_Chain$loadKeysError = F2(
	function (model, error) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				keys: _krisajenkins$remotedata$RemoteData$Failure(error)
			});
	});
var _user$project$Data_Chain$loadKeys = F2(
	function (model, keys) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				keys: _krisajenkins$remotedata$RemoteData$Success(keys)
			});
	});
var _user$project$Data_Chain$loadingKeys = function (model) {
	return _elm_lang$core$Native_Utils.update(
		model,
		{keys: _krisajenkins$remotedata$RemoteData$Loading});
};
var _user$project$Data_Chain$loadingContractIDs = function (model) {
	return _elm_lang$core$Native_Utils.update(
		model,
		{contractIDs: _krisajenkins$remotedata$RemoteData$Loading});
};
var _user$project$Data_Chain$loadContractIDsError = F2(
	function (model, error) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				contractIDs: _krisajenkins$remotedata$RemoteData$Failure(error)
			});
	});
var _user$project$Data_Chain$loadContractIDs = F2(
	function (model, contractIDs) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				contractIDs: _krisajenkins$remotedata$RemoteData$Success(contractIDs)
			});
	});
var _user$project$Data_Chain$loadParsedOperation = F3(
	function (model, operationId, operation) {
		var newParsed = A3(_elm_lang$core$Dict$insert, operationId, operation, model.parsedOperations);
		return _elm_lang$core$Native_Utils.update(
			model,
			{parsedOperations: newParsed});
	});
var _user$project$Data_Chain$loadOperation = F2(
	function (model, operation) {
		var newOperations = A3(_elm_lang$core$Dict$insert, operation.hash, operation, model.operations);
		return _elm_lang$core$Native_Utils.update(
			model,
			{operations: newOperations});
	});
var _user$project$Data_Chain$getBranchList = F2(
	function (model, blockhash) {
		var helper = F2(
			function (hash, blockList) {
				helper:
				while (true) {
					var blockMaybe = A2(_elm_lang$core$Dict$get, hash, model.blocks);
					var _p3 = blockMaybe;
					if (_p3.ctor === 'Just') {
						var _p4 = _p3._0;
						if (_elm_lang$core$Native_Utils.eq(_p4.predecessor, hash)) {
							return blockList;
						} else {
							var _v4 = _p4.predecessor,
								_v5 = {ctor: '::', _0: _p4, _1: blockList};
							hash = _v4;
							blockList = _v5;
							continue helper;
						}
					} else {
						return blockList;
					}
				}
			});
		return _elm_lang$core$List$reverse(
			A2(
				helper,
				blockhash,
				{ctor: '[]'}));
	});
var _user$project$Data_Chain$head = function (model) {
	return _elm_lang$core$List$head(model.heads);
};
var _user$project$Data_Chain$addBlock = F2(
	function (block, blocks) {
		return A3(_elm_lang$core$Dict$insert, block.hash, block, blocks);
	});
var _user$project$Data_Chain$addChainBlocks = F2(
	function (chain, blocks) {
		return A3(_elm_lang$core$List$foldl, _user$project$Data_Chain$addBlock, blocks, chain);
	});
var _user$project$Data_Chain$fitnessGreater = F2(
	function (a, b) {
		fitnessGreater:
		while (true) {
			var _p5 = {ctor: '_Tuple2', _0: a, _1: b};
			_v6_2:
			do {
				if (_p5.ctor === '_Tuple2') {
					if (_p5._0.ctor === '[]') {
						if (_p5._1.ctor === '[]') {
							return false;
						} else {
							break _v6_2;
						}
					} else {
						if (_p5._1.ctor === '::') {
							if (_elm_lang$core$Native_Utils.cmp(_p5._0._0, _p5._1._0) > 0) {
								return true;
							} else {
								var _v7 = _p5._0._1,
									_v8 = _p5._1._1;
								a = _v7;
								b = _v8;
								continue fitnessGreater;
							}
						} else {
							break _v6_2;
						}
					}
				} else {
					break _v6_2;
				}
			} while(false);
			return A2(
				_elm_lang$core$Debug$log,
				A2(
					_elm_lang$core$Basics_ops['++'],
					'error: fitnessGreater: ',
					_elm_lang$core$Basics$toString(
						{ctor: '_Tuple2', _0: a, _1: b})),
				false);
		}
	});
var _user$project$Data_Chain$insertHead = F3(
	function (blocks, newHead, heads) {
		var _p6 = heads;
		if (_p6.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p9 = _p6._1;
			var _p8 = _p6._0;
			var blockMaybe = A2(_elm_lang$core$Dict$get, _p8, blocks);
			var _p7 = blockMaybe;
			if (_p7.ctor === 'Just') {
				return A2(_user$project$Data_Chain$fitnessGreater, newHead.fitness, _p7._0.fitness) ? {
					ctor: '::',
					_0: newHead.hash,
					_1: {ctor: '::', _0: _p8, _1: _p9}
				} : {
					ctor: '::',
					_0: _p8,
					_1: A3(_user$project$Data_Chain$insertHead, blocks, newHead, _p9)
				};
			} else {
				return A2(_elm_lang$core$Debug$log, 'error: insertHead failed', heads);
			}
		}
	});
var _user$project$Data_Chain$updateExistingHead = F3(
	function (newHead, predHash, heads) {
		return A2(
			_elm_lang$core$Maybe$map,
			function (_p10) {
				return A3(
					_elm_community$list_extra$List_Extra$updateIf,
					function (h) {
						return _elm_lang$core$Native_Utils.eq(h, predHash);
					},
					function (_p11) {
						return newHead;
					},
					heads);
			},
			A2(
				_elm_community$list_extra$List_Extra$findIndex,
				function (h) {
					return _elm_lang$core$Native_Utils.eq(h, predHash);
				},
				heads));
	});
var _user$project$Data_Chain$updateHeads = F3(
	function (blocks, newChain, heads) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			heads,
			A2(
				_elm_lang$core$Maybe$andThen,
				function (newhead) {
					return A2(
						_elm_lang$core$Maybe$map,
						function (newpredhash) {
							var _p12 = A3(_user$project$Data_Chain$updateExistingHead, newhead.hash, newpredhash, heads);
							if (_p12.ctor === 'Just') {
								return _p12._0;
							} else {
								return A3(_user$project$Data_Chain$insertHead, blocks, newhead, heads);
							}
						},
						A2(
							_elm_lang$core$Maybe$map,
							function (_) {
								return _.predecessor;
							},
							_elm_community$list_extra$List_Extra$last(newChain)));
				},
				_elm_lang$core$List$head(newChain)));
	});
var _user$project$Data_Chain$loadBlocks = F2(
	function (model, blocksData) {
		var newBlocks = A3(_elm_lang$core$List$foldl, _user$project$Data_Chain$addChainBlocks, model.blocks, blocksData);
		return _elm_lang$core$Native_Utils.update(
			model,
			{blocks: newBlocks});
	});
var _user$project$Data_Chain$loadHeads = F2(
	function (model, headsData) {
		var heads = A2(
			_elm_lang$core$List$map,
			function (_) {
				return _.hash;
			},
			A2(
				_elm_lang$core$List$filterMap,
				_elm_lang$core$Basics$identity,
				A2(_elm_lang$core$List$map, _elm_lang$core$List$head, headsData)));
		var newModel = A2(_user$project$Data_Chain$loadBlocks, model, headsData);
		return _elm_lang$core$Native_Utils.update(
			newModel,
			{heads: heads});
	});
var _user$project$Data_Chain$updateMonitor = F2(
	function (model, headsData) {
		var newModel = A2(_user$project$Data_Chain$loadBlocks, model, headsData);
		var newHeads = A3(
			_elm_lang$core$List$foldr,
			_user$project$Data_Chain$updateHeads(newModel.blocks),
			newModel.heads,
			headsData);
		return _elm_lang$core$Native_Utils.update(
			newModel,
			{heads: newHeads});
	});
var _user$project$Data_Chain$addBlockOperations = F3(
	function (model, blockhash, operations) {
		var addOperation = F2(
			function (operation, dict) {
				return A3(_elm_lang$core$Dict$insert, operation.hash, operation, dict);
			});
		var parsedOperations = A3(
			_elm_lang$core$List$foldr,
			addOperation,
			model.parsedOperations,
			_elm_lang$core$List$concat(operations));
		var blockOperations = A3(
			_elm_lang$core$Dict$insert,
			blockhash,
			A2(
				_elm_lang$core$List$map,
				function (_) {
					return _.hash;
				},
				_elm_lang$core$List$concat(operations)),
			model.blockOperations);
		return _elm_lang$core$Native_Utils.update(
			model,
			{blockOperations: blockOperations, parsedOperations: parsedOperations});
	});
var _user$project$Data_Chain$blocksNeedingOperations = function (model) {
	var blockOperHashSet = _elm_lang$core$Set$fromList(
		A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			_elm_lang$core$Dict$toList(model.blockOperations)));
	var blockHashSet = _elm_lang$core$Set$fromList(
		A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			_elm_lang$core$Dict$toList(model.blocks)));
	return _elm_lang$core$Set$toList(
		A2(_elm_lang$core$Set$diff, blockHashSet, blockOperHashSet));
};
var _user$project$Data_Chain$blockNeedsOperations = F2(
	function (model, blockHash) {
		return !A2(_elm_lang$core$Dict$member, blockHash, model.blockOperations);
	});
var _user$project$Data_Chain$init = {
	heads: {ctor: '[]'},
	blocks: _elm_lang$core$Dict$empty,
	operations: _elm_lang$core$Dict$empty,
	parsedOperations: _elm_lang$core$Dict$empty,
	blockOperations: _elm_lang$core$Dict$empty,
	contractIDs: _krisajenkins$remotedata$RemoteData$NotAsked,
	keys: _krisajenkins$remotedata$RemoteData$NotAsked,
	peers: _krisajenkins$remotedata$RemoteData$NotAsked,
	contracts: _elm_lang$core$Dict$empty
};
var _user$project$Data_Chain$Block = F7(
	function (a, b, c, d, e, f, g) {
		return {hash: a, predecessor: b, fitness: c, timestamp: d, operations: e, net_id: f, level: g};
	});
var _user$project$Data_Chain$decodeBlock = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'level',
	_elm_lang$core$Json_Decode$int,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'net_id',
		_elm_lang$core$Json_Decode$string,
		A4(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional,
			'operations',
			A2(
				_elm_lang$core$Json_Decode$map,
				_elm_lang$core$Maybe$Just,
				_elm_lang$core$Json_Decode$list(
					_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string))),
			_elm_lang$core$Maybe$Nothing,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'timestamp',
				_user$project$Data_Chain$decodeTimestamp,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
					'fitness',
					_user$project$Data_Chain$decodeFitness,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'predecessor',
						_elm_lang$core$Json_Decode$string,
						A3(
							_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
							'hash',
							_elm_lang$core$Json_Decode$string,
							_elm_lang$core$Json_Decode$succeed(_user$project$Data_Chain$Block))))))));
var _user$project$Data_Chain$decodeBlocks = A2(
	_elm_lang$core$Json_Decode$field,
	'blocks',
	_elm_lang$core$Json_Decode$list(
		_elm_lang$core$Json_Decode$list(_user$project$Data_Chain$decodeBlock)));
var _user$project$Data_Chain$Operation = F3(
	function (a, b, c) {
		return {hash: a, netID: b, data: c};
	});
var _user$project$Data_Chain$ParsedOperation = F6(
	function (a, b, c, d, e, f) {
		return {hash: a, net_id: b, branch: c, operations: d, source: e, signature: f};
	});
var _user$project$Data_Chain$Key = F2(
	function (a, b) {
		return {hash: a, public_key: b};
	});
var _user$project$Data_Chain$decodeKey = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'public_key',
	_elm_lang$core$Json_Decode$string,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'hash',
		_elm_lang$core$Json_Decode$string,
		_elm_lang$core$Json_Decode$succeed(_user$project$Data_Chain$Key)));
var _user$project$Data_Chain$decodeKeys = A2(
	_elm_lang$core$Json_Decode$field,
	'ok',
	_elm_lang$core$Json_Decode$list(_user$project$Data_Chain$decodeKey));
var _user$project$Data_Chain$Peer = F2(
	function (a, b) {
		return {hash: a, info: b};
	});
var _user$project$Data_Chain$PeerInfo = F6(
	function (a, b, c, d, e, f) {
		return {value: a, state: b, stat: c, trusted: d, score: e, lastConnection: f};
	});
var _user$project$Data_Chain$Connection = F2(
	function (a, b) {
		return {addr: a, time: b};
	});
var _user$project$Data_Chain$Address = F2(
	function (a, b) {
		return {addr: a, port_: b};
	});
var _user$project$Data_Chain$decodeAddress = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'port',
	_elm_lang$core$Json_Decode$int,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'addr',
		_elm_lang$core$Json_Decode$string,
		_elm_lang$core$Json_Decode$succeed(_user$project$Data_Chain$Address)));
var _user$project$Data_Chain$PeerStats = F4(
	function (a, b, c, d) {
		return {total_sent: a, total_recv: b, current_inflow: c, current_outflow: d};
	});
var _user$project$Data_Chain$decodePeerStats = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'current_outflow',
	_elm_lang$core$Json_Decode$int,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'current_inflow',
		_elm_lang$core$Json_Decode$int,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'total_recv',
			_elm_lang$core$Json_Decode$int,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'total_sent',
				_elm_lang$core$Json_Decode$int,
				_elm_lang$core$Json_Decode$succeed(_user$project$Data_Chain$PeerStats)))));
var _user$project$Data_Chain$Delegate = F2(
	function (a, b) {
		return {setable: a, value: b};
	});
var _user$project$Data_Chain$decodeDelegate = A4(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional,
	'value',
	A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _elm_lang$core$Json_Decode$string),
	_elm_lang$core$Maybe$Nothing,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'setable',
		_elm_lang$core$Json_Decode$bool,
		_elm_lang$core$Json_Decode$succeed(_user$project$Data_Chain$Delegate)));
var _user$project$Data_Chain$Contract = F8(
	function (a, b, c, d, e, f, g, h) {
		return {raw: a, counter: b, balance: c, spendable: d, manager: e, delegate: f, script: g, rawBody: h};
	});
var _user$project$Data_Chain$decodeContract = A2(
	_elm_lang$core$Json_Decode$andThen,
	function (raw) {
		return A2(
			_elm_lang$core$Json_Decode$field,
			'ok',
			A2(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$hardcoded,
				'[placeholder]',
				A4(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional,
					'script',
					A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _user$project$Data_Michelson$decodeScript),
					_elm_lang$core$Maybe$Nothing,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'delegate',
						_user$project$Data_Chain$decodeDelegate,
						A3(
							_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
							'manager',
							_elm_lang$core$Json_Decode$string,
							A3(
								_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
								'spendable',
								_elm_lang$core$Json_Decode$bool,
								A3(
									_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
									'balance',
									_user$project$Data_Chain$decodeBalance,
									A3(
										_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
										'counter',
										_elm_lang$core$Json_Decode$int,
										A2(
											_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$hardcoded,
											raw,
											_elm_lang$core$Json_Decode$succeed(_user$project$Data_Chain$Contract))))))))));
	},
	_elm_lang$core$Json_Decode$value);
var _user$project$Data_Chain$decodeContractResponse = function (response) {
	return A2(
		_elm_lang$core$Result$map,
		function (contract) {
			return _elm_lang$core$Native_Utils.update(
				contract,
				{rawBody: response.body});
		},
		A2(_elm_lang$core$Json_Decode$decodeString, _user$project$Data_Chain$decodeContract, response.body));
};
var _user$project$Data_Chain$Model = F9(
	function (a, b, c, d, e, f, g, h, i) {
		return {heads: a, blocks: b, operations: c, parsedOperations: d, blockOperations: e, contractIDs: f, keys: g, peers: h, contracts: i};
	});
var _user$project$Data_Chain$Delegation = function (a) {
	return {ctor: 'Delegation', _0: a};
};
var _user$project$Data_Chain$Faucet = F2(
	function (a, b) {
		return {ctor: 'Faucet', _0: a, _1: b};
	});
var _user$project$Data_Chain$Transaction = F2(
	function (a, b) {
		return {ctor: 'Transaction', _0: a, _1: b};
	});
var _user$project$Data_Chain$SeedNonceRevelation = F2(
	function (a, b) {
		return {ctor: 'SeedNonceRevelation', _0: a, _1: b};
	});
var _user$project$Data_Chain$Endorsement = F2(
	function (a, b) {
		return {ctor: 'Endorsement', _0: a, _1: b};
	});
var _user$project$Data_Chain$Unknown = function (a) {
	return {ctor: 'Unknown', _0: a};
};
var _user$project$Data_Chain$ItemValue = function (a) {
	return {ctor: 'ItemValue', _0: a};
};
var _user$project$Data_Chain$ItemString = function (a) {
	return {ctor: 'ItemString', _0: a};
};
var _user$project$Data_Chain$AddressAddr = function (a) {
	return {ctor: 'AddressAddr', _0: a};
};
var _user$project$Data_Chain$AddressTime = function (a) {
	return {ctor: 'AddressTime', _0: a};
};
var _user$project$Data_Chain$decodeConnection = function () {
	var decodeAddrItem = _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: A2(_elm_lang$core$Json_Decode$map, _user$project$Data_Chain$AddressTime, _user$project$Data_Chain$decodeTimestamp),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _user$project$Data_Chain$AddressAddr, _user$project$Data_Chain$decodeAddress),
				_1: {ctor: '[]'}
			}
		});
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		function (list) {
			var _p13 = list;
			if (((((_p13.ctor === '::') && (_p13._0.ctor === 'AddressAddr')) && (_p13._1.ctor === '::')) && (_p13._1._0.ctor === 'AddressTime')) && (_p13._1._1.ctor === '[]')) {
				return _elm_lang$core$Json_Decode$succeed(
					A2(_user$project$Data_Chain$Connection, _p13._0._0, _p13._1._0._0));
			} else {
				return _elm_lang$core$Json_Decode$fail('address decode failed');
			}
		},
		_elm_lang$core$Json_Decode$list(decodeAddrItem));
}();
var _user$project$Data_Chain$decodePeerInfo = A2(
	_elm_lang$core$Json_Decode$andThen,
	function (value) {
		return A4(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional,
			'last_established_connection',
			A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _user$project$Data_Chain$decodeConnection),
			_elm_lang$core$Maybe$Nothing,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'score',
				_elm_lang$core$Json_Decode$int,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
					'trusted',
					_elm_lang$core$Json_Decode$bool,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'stat',
						_user$project$Data_Chain$decodePeerStats,
						A3(
							_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
							'state',
							_elm_lang$core$Json_Decode$string,
							A2(
								_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$hardcoded,
								value,
								_elm_lang$core$Json_Decode$succeed(_user$project$Data_Chain$PeerInfo)))))));
	},
	_elm_lang$core$Json_Decode$value);
var _user$project$Data_Chain$decodePeer = function () {
	var decodeValue = A2(_elm_lang$core$Json_Decode$map, _user$project$Data_Chain$ItemValue, _user$project$Data_Chain$decodePeerInfo);
	var decodeString = A2(_elm_lang$core$Json_Decode$map, _user$project$Data_Chain$ItemString, _elm_lang$core$Json_Decode$string);
	var decodeItem = _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: decodeString,
			_1: {
				ctor: '::',
				_0: decodeValue,
				_1: {ctor: '[]'}
			}
		});
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		function (list) {
			var _p14 = list;
			if (((((_p14.ctor === '::') && (_p14._0.ctor === 'ItemString')) && (_p14._1.ctor === '::')) && (_p14._1._0.ctor === 'ItemValue')) && (_p14._1._1.ctor === '[]')) {
				return _elm_lang$core$Json_Decode$succeed(
					A2(_user$project$Data_Chain$Peer, _p14._0._0, _p14._1._0._0));
			} else {
				return _elm_lang$core$Json_Decode$fail('bad peer');
			}
		},
		_elm_lang$core$Json_Decode$list(decodeItem));
}();
var _user$project$Data_Chain$decodePeers = _elm_lang$core$Json_Decode$list(_user$project$Data_Chain$decodePeer);

var _user$project$Data_Request$emptyJsonBody = _elm_lang$http$Http$jsonBody(
	_elm_lang$core$Json_Encode$object(
		{ctor: '[]'}));

var _user$project$Data_Schema$schemaError = F2(
	function (msg, data) {
		var _p0 = A2(_elm_lang$core$Debug$log, 'schema error:', msg);
		return data;
	});
var _user$project$Data_Schema$margin = _elm_lang$html$Html_Attributes$style(
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'margin-left', _1: '1.5em'},
		_1: {ctor: '[]'}
	});
var _user$project$Data_Schema$listGlyph = function () {
	var bulletString = _elm_lang$core$String$fromChar(
		_elm_lang$core$Char$fromCode(9679));
	return A2(
		_elm_lang$html$Html$span,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('bullet'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(bulletString),
			_1: {ctor: '[]'}
		});
}();
var _user$project$Data_Schema$viewSchemaDataRaw = function (schemaDataMaybe) {
	var _p1 = schemaDataMaybe;
	if (_p1.ctor === 'Just') {
		return A2(
			_elm_lang$html$Html$pre,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'white-space', _1: 'pre-wrap'},
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(
					_elm_lang$core$Basics$toString(_p1._0)),
				_1: {ctor: '[]'}
			});
	} else {
		return _elm_lang$html$Html$text('[no schema data]');
	}
};
var _user$project$Data_Schema$ClickField = function (a) {
	return {ctor: 'ClickField', _0: a};
};
var _user$project$Data_Schema$SchemaNull = {ctor: 'SchemaNull'};
var _user$project$Data_Schema$SchemaBool = function (a) {
	return {ctor: 'SchemaBool', _0: a};
};
var _user$project$Data_Schema$SchemaInt = function (a) {
	return {ctor: 'SchemaInt', _0: a};
};
var _user$project$Data_Schema$SchemaString = function (a) {
	return {ctor: 'SchemaString', _0: a};
};
var _user$project$Data_Schema$SchemaList = function (a) {
	return {ctor: 'SchemaList', _0: a};
};
var _user$project$Data_Schema$makeList = function (items) {
	return _user$project$Data_Schema$SchemaList(
		A2(
			_elm_lang$core$List$map,
			function (item) {
				return {ctor: '_Tuple2', _0: true, _1: item};
			},
			items));
};
var _user$project$Data_Schema$SchemaObject = function (a) {
	return {ctor: 'SchemaObject', _0: a};
};
var _user$project$Data_Schema$makeObject = function (fields) {
	var fieldsWithVisible = A2(
		_elm_lang$core$List$map,
		function (_p2) {
			var _p3 = _p2;
			return {
				ctor: '_Tuple2',
				_0: _p3._0,
				_1: {ctor: '_Tuple2', _0: true, _1: _p3._1}
			};
		},
		fields);
	return _user$project$Data_Schema$SchemaObject(
		_elm_lang$core$Dict$fromList(fieldsWithVisible));
};
var _user$project$Data_Schema$decodeSchema = _elm_lang$core$Json_Decode$oneOf(
	{
		ctor: '::',
		_0: A2(
			_elm_lang$core$Json_Decode$map,
			_user$project$Data_Schema$makeObject,
			_elm_lang$core$Json_Decode$keyValuePairs(
				_elm_lang$core$Json_Decode$lazy(
					function (_p4) {
						return _user$project$Data_Schema$decodeSchema;
					}))),
		_1: {
			ctor: '::',
			_0: A2(
				_elm_lang$core$Json_Decode$map,
				_user$project$Data_Schema$makeList,
				_elm_lang$core$Json_Decode$list(
					_elm_lang$core$Json_Decode$lazy(
						function (_p5) {
							return _user$project$Data_Schema$decodeSchema;
						}))),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _user$project$Data_Schema$SchemaString, _elm_lang$core$Json_Decode$string),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$Json_Decode$map, _user$project$Data_Schema$SchemaInt, _elm_lang$core$Json_Decode$int),
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$core$Json_Decode$map, _user$project$Data_Schema$SchemaBool, _elm_lang$core$Json_Decode$bool),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Json_Decode$null(_user$project$Data_Schema$SchemaNull),
							_1: {ctor: '[]'}
						}
					}
				}
			}
		}
	});
var _user$project$Data_Schema$toggleVisible = F2(
	function (context, schemaData) {
		var _p6 = context;
		if (_p6.ctor === '[]') {
			return A2(_user$project$Data_Schema$schemaError, 'ran out of context', schemaData);
		} else {
			var _p19 = _p6._1;
			var _p18 = _p6._0;
			var _p7 = schemaData;
			switch (_p7.ctor) {
				case 'SchemaObject':
					var toggle = function (_p8) {
						var _p9 = _p8;
						var _p11 = _p9._0;
						var _p10 = _p9._1;
						return _elm_lang$core$Native_Utils.eq(
							_p19,
							{ctor: '[]'}) ? {ctor: '_Tuple2', _0: !_p11, _1: _p10} : {
							ctor: '_Tuple2',
							_0: _p11,
							_1: A2(_user$project$Data_Schema$toggleVisible, _p19, _p10)
						};
					};
					var newFields = function (fieldName) {
						return A3(
							_elm_lang$core$Dict$update,
							fieldName,
							_elm_lang$core$Maybe$map(toggle),
							_p7._0);
					};
					var _p12 = _p18;
					if (_p12.ctor === 'FieldName') {
						return _user$project$Data_Schema$SchemaObject(
							newFields(_p12._0));
					} else {
						return A2(
							_user$project$Data_Schema$schemaError,
							A2(
								_elm_lang$core$Basics_ops['++'],
								'no match for ',
								_elm_lang$core$Basics$toString(context)),
							schemaData);
					}
				case 'SchemaList':
					var toggle = F3(
						function (i, contextIndex, _p13) {
							var _p14 = _p13;
							var _p16 = _p14._0;
							var _p15 = _p14._1;
							return _elm_lang$core$Native_Utils.eq(i, contextIndex) ? (_elm_lang$core$Native_Utils.eq(
								_p19,
								{ctor: '[]'}) ? {ctor: '_Tuple2', _0: !_p16, _1: _p15} : {
								ctor: '_Tuple2',
								_0: _p16,
								_1: A2(_user$project$Data_Schema$toggleVisible, _p19, _p15)
							}) : {ctor: '_Tuple2', _0: _p16, _1: _p15};
						});
					var _p17 = _p18;
					if (_p17.ctor === 'ListIndex') {
						return _user$project$Data_Schema$SchemaList(
							A2(
								_elm_lang$core$List$indexedMap,
								toggle(_p17._0),
								_p7._0));
					} else {
						return A2(
							_user$project$Data_Schema$schemaError,
							A2(
								_elm_lang$core$Basics_ops['++'],
								'no match for ',
								_elm_lang$core$Basics$toString(context)),
							schemaData);
					}
				default:
					return A2(
						_user$project$Data_Schema$schemaError,
						A2(
							_elm_lang$core$Basics_ops['++'],
							'no match for ',
							_elm_lang$core$Basics$toString(context)),
						schemaData);
			}
		}
	});
var _user$project$Data_Schema$update = F2(
	function (msg, schemaData) {
		var _p20 = msg;
		return A2(
			_user$project$Data_Schema$toggleVisible,
			_elm_lang$core$List$reverse(_p20._0),
			schemaData);
	});
var _user$project$Data_Schema$mapSchemaData = F2(
	function (fn, schemaData) {
		var _p21 = schemaData;
		switch (_p21.ctor) {
			case 'SchemaObject':
				return _user$project$Data_Schema$SchemaObject(
					A2(
						_elm_lang$core$Dict$map,
						F2(
							function (_p23, _p22) {
								var _p24 = _p22;
								return fn(
									{
										ctor: '_Tuple2',
										_0: _p24._0,
										_1: A2(_user$project$Data_Schema$mapSchemaData, fn, _p24._1)
									});
							}),
						_p21._0));
			case 'SchemaList':
				return _user$project$Data_Schema$SchemaList(
					A2(
						_elm_lang$core$List$map,
						function (_p25) {
							var _p26 = _p25;
							return fn(
								{
									ctor: '_Tuple2',
									_0: _p26._0,
									_1: A2(_user$project$Data_Schema$mapSchemaData, fn, _p26._1)
								});
						},
						_p21._0));
			default:
				return schemaData;
		}
	});
var _user$project$Data_Schema$collapseAll = function (schemaData) {
	return A2(
		_user$project$Data_Schema$mapSchemaData,
		function (_p27) {
			var _p28 = _p27;
			return {ctor: '_Tuple2', _0: false, _1: _p28._1};
		},
		schemaData);
};
var _user$project$Data_Schema$collapseTrees = function (schemaData) {
	var _p29 = schemaData;
	switch (_p29.ctor) {
		case 'SchemaObject':
			return _user$project$Data_Schema$SchemaObject(
				A2(_elm_lang$core$Dict$map, _user$project$Data_Schema$objectMap, _p29._0));
		case 'SchemaList':
			return _user$project$Data_Schema$SchemaList(
				A2(
					_elm_lang$core$List$map,
					function (_p30) {
						var _p31 = _p30;
						return {
							ctor: '_Tuple2',
							_0: _p31._0,
							_1: _user$project$Data_Schema$collapseTrees(_p31._1)
						};
					},
					_p29._0));
		default:
			return schemaData;
	}
};
var _user$project$Data_Schema$objectMap = F2(
	function (name, _p32) {
		var _p33 = _p32;
		return {
			ctor: '_Tuple2',
			_0: _p33._0 && (!_elm_lang$core$Native_Utils.eq(name, 'tree')),
			_1: _user$project$Data_Schema$collapseTrees(_p33._1)
		};
	});
var _user$project$Data_Schema$ListIndex = function (a) {
	return {ctor: 'ListIndex', _0: a};
};
var _user$project$Data_Schema$FieldName = function (a) {
	return {ctor: 'FieldName', _0: a};
};
var _user$project$Data_Schema$viewSchemaObject = F2(
	function (context, properties) {
		var viewField = function (_p34) {
			var _p35 = _p34;
			var _p38 = _p35._1._1;
			var _p37 = _p35._0;
			var labelSuffix = function () {
				var _p36 = _p38;
				switch (_p36.ctor) {
					case 'SchemaObject':
						return '{';
					case 'SchemaList':
						return '[';
					default:
						return ':';
				}
			}();
			var newContext = {
				ctor: '::',
				_0: _user$project$Data_Schema$FieldName(_p37),
				_1: context
			};
			return A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _user$project$Data_Schema$margin,
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$classList(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'collapsed', _1: !_p35._1._0},
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('fieldlabel'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Events$onClick(
									_user$project$Data_Schema$ClickField(newContext)),
								_1: {ctor: '[]'}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(_p37),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('suffix'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(
									A2(
										_elm_lang$core$Basics_ops['++'],
										' ',
										A2(_elm_lang$core$Basics_ops['++'], labelSuffix, ' '))),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(_user$project$Data_Schema$viewSchemaData, newContext, _p38),
							_1: {ctor: '[]'}
						}
					}
				});
		};
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('object'),
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$List$map,
				viewField,
				_elm_lang$core$Dict$toList(properties)));
	});
var _user$project$Data_Schema$viewSchemaData = F2(
	function (context, schemaData) {
		var _p39 = schemaData;
		switch (_p39.ctor) {
			case 'SchemaObject':
				return A2(_user$project$Data_Schema$viewSchemaObject, context, _p39._0);
			case 'SchemaList':
				return A2(_user$project$Data_Schema$viewSchemaList, context, _p39._0);
			case 'SchemaString':
				return A2(
					_elm_lang$html$Html$span,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('string'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'\"',
								A2(_elm_lang$core$Basics_ops['++'], _p39._0, '\"'))),
						_1: {ctor: '[]'}
					});
			case 'SchemaInt':
				return A2(
					_elm_lang$html$Html$span,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('int'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_elm_lang$core$Basics$toString(_p39._0)),
						_1: {ctor: '[]'}
					});
			case 'SchemaBool':
				return A2(
					_elm_lang$html$Html$span,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('bool'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_elm_lang$core$Basics$toString(_p39._0)),
						_1: {ctor: '[]'}
					});
			default:
				return A2(
					_elm_lang$html$Html$span,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('null'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('null'),
						_1: {ctor: '[]'}
					});
		}
	});
var _user$project$Data_Schema$viewSchemaList = F2(
	function (context, items) {
		var viewItem = F2(
			function (i, _p40) {
				var _p41 = _p40;
				var newContext = {
					ctor: '::',
					_0: _user$project$Data_Schema$ListIndex(i),
					_1: context
				};
				return A2(
					_elm_lang$html$Html$li,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$classList(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'collapsed', _1: !_p41._0},
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('listmark'),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Events$onClick(
										_user$project$Data_Schema$ClickField(newContext)),
									_1: {ctor: '[]'}
								}
							},
							{
								ctor: '::',
								_0: _user$project$Data_Schema$listGlyph,
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_user$project$Data_Schema$viewSchemaData,
								{
									ctor: '::',
									_0: _user$project$Data_Schema$ListIndex(i),
									_1: context
								},
								_p41._1),
							_1: {ctor: '[]'}
						}
					});
			});
		return A2(
			_elm_lang$html$Html$ul,
			{
				ctor: '::',
				_0: _user$project$Data_Schema$margin,
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('list'),
					_1: {ctor: '[]'}
				}
			},
			A2(_elm_lang$core$List$indexedMap, viewItem, items));
	});
var _user$project$Data_Schema$viewSchemaDataTop = F2(
	function (schemaQuery, data) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('schemadata'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$h2,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							A2(_elm_lang$core$Basics_ops['++'], 'Raw Schema for ', schemaQuery)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$p,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('Here is a view of the JSON Schema for the Tezos RPC API. Click labels and bullets to collapse and expand'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_user$project$Data_Schema$viewSchemaData,
							{ctor: '[]'},
							data),
						_1: {ctor: '[]'}
					}
				}
			});
	});

var _user$project$Route$routeToString = function (route) {
	var pieces = function () {
		var _p0 = route;
		switch (_p0.ctor) {
			case 'Home':
				return {ctor: '[]'};
			case 'Block':
				return {
					ctor: '::',
					_0: 'block',
					_1: {
						ctor: '::',
						_0: _p0._0,
						_1: {ctor: '[]'}
					}
				};
			case 'Operations':
				return {
					ctor: '::',
					_0: 'operations',
					_1: {ctor: '[]'}
				};
			case 'Operation':
				return {
					ctor: '::',
					_0: 'operation',
					_1: {
						ctor: '::',
						_0: _p0._0,
						_1: {ctor: '[]'}
					}
				};
			case 'ChainAt':
				return {
					ctor: '::',
					_0: 'chainat',
					_1: {
						ctor: '::',
						_0: _p0._0,
						_1: {ctor: '[]'}
					}
				};
			case 'Contracts':
				return {
					ctor: '::',
					_0: 'contracts',
					_1: {ctor: '[]'}
				};
			case 'Keys':
				return {
					ctor: '::',
					_0: 'keys',
					_1: {ctor: '[]'}
				};
			case 'Peers':
				return {
					ctor: '::',
					_0: 'peers',
					_1: {ctor: '[]'}
				};
			case 'Contract':
				return {
					ctor: '::',
					_0: 'contract',
					_1: {
						ctor: '::',
						_0: _p0._0,
						_1: {ctor: '[]'}
					}
				};
			case 'Schema':
				return {
					ctor: '::',
					_0: 'schema',
					_1: {ctor: '[]'}
				};
			case 'Heads':
				return {
					ctor: '::',
					_0: 'heads',
					_1: {ctor: '[]'}
				};
			case 'Debug':
				return {
					ctor: '::',
					_0: 'debug',
					_1: {ctor: '[]'}
				};
			case 'Errors':
				return {
					ctor: '::',
					_0: 'errors',
					_1: {ctor: '[]'}
				};
			default:
				return {
					ctor: '::',
					_0: 'about',
					_1: {ctor: '[]'}
				};
		}
	}();
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'#/',
		A2(_elm_lang$core$String$join, '/', pieces));
};
var _user$project$Route$href = function (route) {
	return _elm_lang$html$Html_Attributes$href(
		_user$project$Route$routeToString(route));
};
var _user$project$Route$modifyUrl = function (_p1) {
	return _elm_lang$navigation$Navigation$modifyUrl(
		_user$project$Route$routeToString(_p1));
};
var _user$project$Route$newUrl = function (_p2) {
	return _elm_lang$navigation$Navigation$newUrl(
		_user$project$Route$routeToString(_p2));
};
var _user$project$Route$About = {ctor: 'About'};
var _user$project$Route$Debug = {ctor: 'Debug'};
var _user$project$Route$Errors = {ctor: 'Errors'};
var _user$project$Route$Heads = {ctor: 'Heads'};
var _user$project$Route$Schema = {ctor: 'Schema'};
var _user$project$Route$Contract = function (a) {
	return {ctor: 'Contract', _0: a};
};
var _user$project$Route$Peers = {ctor: 'Peers'};
var _user$project$Route$Keys = {ctor: 'Keys'};
var _user$project$Route$Contracts = {ctor: 'Contracts'};
var _user$project$Route$ChainAt = function (a) {
	return {ctor: 'ChainAt', _0: a};
};
var _user$project$Route$Operation = function (a) {
	return {ctor: 'Operation', _0: a};
};
var _user$project$Route$Operations = {ctor: 'Operations'};
var _user$project$Route$Block = function (a) {
	return {ctor: 'Block', _0: a};
};
var _user$project$Route$Home = {ctor: 'Home'};
var _user$project$Route$route = _evancz$url_parser$UrlParser$oneOf(
	{
		ctor: '::',
		_0: A2(
			_evancz$url_parser$UrlParser$map,
			_user$project$Route$Home,
			_evancz$url_parser$UrlParser$s('')),
		_1: {
			ctor: '::',
			_0: A2(
				_evancz$url_parser$UrlParser$map,
				_user$project$Route$Home,
				_evancz$url_parser$UrlParser$s('home')),
			_1: {
				ctor: '::',
				_0: A2(
					_evancz$url_parser$UrlParser$map,
					_user$project$Route$Heads,
					_evancz$url_parser$UrlParser$s('heads')),
				_1: {
					ctor: '::',
					_0: A2(
						_evancz$url_parser$UrlParser$map,
						_user$project$Route$Block,
						A2(
							_evancz$url_parser$UrlParser_ops['</>'],
							_evancz$url_parser$UrlParser$s('block'),
							_evancz$url_parser$UrlParser$string)),
					_1: {
						ctor: '::',
						_0: A2(
							_evancz$url_parser$UrlParser$map,
							_user$project$Route$Operations,
							_evancz$url_parser$UrlParser$s('operations')),
						_1: {
							ctor: '::',
							_0: A2(
								_evancz$url_parser$UrlParser$map,
								_user$project$Route$Operation,
								A2(
									_evancz$url_parser$UrlParser_ops['</>'],
									_evancz$url_parser$UrlParser$s('operation'),
									_evancz$url_parser$UrlParser$string)),
							_1: {
								ctor: '::',
								_0: A2(
									_evancz$url_parser$UrlParser$map,
									_user$project$Route$ChainAt,
									A2(
										_evancz$url_parser$UrlParser_ops['</>'],
										_evancz$url_parser$UrlParser$s('chainat'),
										_evancz$url_parser$UrlParser$string)),
								_1: {
									ctor: '::',
									_0: A2(
										_evancz$url_parser$UrlParser$map,
										_user$project$Route$Contracts,
										_evancz$url_parser$UrlParser$s('contracts')),
									_1: {
										ctor: '::',
										_0: A2(
											_evancz$url_parser$UrlParser$map,
											_user$project$Route$Keys,
											_evancz$url_parser$UrlParser$s('keys')),
										_1: {
											ctor: '::',
											_0: A2(
												_evancz$url_parser$UrlParser$map,
												_user$project$Route$Peers,
												_evancz$url_parser$UrlParser$s('peers')),
											_1: {
												ctor: '::',
												_0: A2(
													_evancz$url_parser$UrlParser$map,
													_user$project$Route$Contract,
													A2(
														_evancz$url_parser$UrlParser_ops['</>'],
														_evancz$url_parser$UrlParser$s('contract'),
														_evancz$url_parser$UrlParser$string)),
												_1: {
													ctor: '::',
													_0: A2(
														_evancz$url_parser$UrlParser$map,
														_user$project$Route$Schema,
														_evancz$url_parser$UrlParser$s('schema')),
													_1: {
														ctor: '::',
														_0: A2(
															_evancz$url_parser$UrlParser$map,
															_user$project$Route$Debug,
															_evancz$url_parser$UrlParser$s('debug')),
														_1: {
															ctor: '::',
															_0: A2(
																_evancz$url_parser$UrlParser$map,
																_user$project$Route$Errors,
																_evancz$url_parser$UrlParser$s('errors')),
															_1: {
																ctor: '::',
																_0: A2(
																	_evancz$url_parser$UrlParser$map,
																	_user$project$Route$About,
																	_evancz$url_parser$UrlParser$s('about')),
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
				}
			}
		}
	});
var _user$project$Route$fromLocation = function (location) {
	return _elm_lang$core$String$isEmpty(location.hash) ? _elm_lang$core$Maybe$Just(_user$project$Route$Home) : A2(_evancz$url_parser$UrlParser$parseHash, _user$project$Route$route, location);
};

var _user$project$Page$About = {ctor: 'About'};
var _user$project$Page$Debug = {ctor: 'Debug'};
var _user$project$Page$Errors = {ctor: 'Errors'};
var _user$project$Page$Contract = function (a) {
	return {ctor: 'Contract', _0: a};
};
var _user$project$Page$Peers = {ctor: 'Peers'};
var _user$project$Page$Keys = {ctor: 'Keys'};
var _user$project$Page$Contracts = {ctor: 'Contracts'};
var _user$project$Page$ChainAt = function (a) {
	return {ctor: 'ChainAt', _0: a};
};
var _user$project$Page$Operation = function (a) {
	return {ctor: 'Operation', _0: a};
};
var _user$project$Page$Operations = {ctor: 'Operations'};
var _user$project$Page$Schema = {ctor: 'Schema'};
var _user$project$Page$Block = function (a) {
	return {ctor: 'Block', _0: a};
};
var _user$project$Page$Home = {ctor: 'Home'};
var _user$project$Page$Heads = {ctor: 'Heads'};
var _user$project$Page$NotFound = {ctor: 'NotFound'};
var _user$project$Page$Blank = {ctor: 'Blank'};

var _user$project$Model$getPage = function (_p0) {
	var _p1 = _p0;
	return _p1._0;
};
var _user$project$Model$Model = F6(
	function (a, b, c, d, e, f) {
		return {schemaData: a, errors: b, nodeUrl: c, now: d, chain: e, pageState: f};
	});
var _user$project$Model$Loaded = function (a) {
	return {ctor: 'Loaded', _0: a};
};
var _user$project$Model$OtherError = function (a) {
	return {ctor: 'OtherError', _0: a};
};
var _user$project$Model$HttpError = function (a) {
	return {ctor: 'HttpError', _0: a};
};

var _user$project$Request_Block$getContract = F2(
	function (nodeUrl, contractId) {
		var url = A2(
			_elm_lang$core$Basics_ops['++'],
			nodeUrl,
			A2(_elm_lang$core$Basics_ops['++'], '/blocks/head/proto/context/contracts/', contractId));
		return _elm_lang$http$Http$request(
			{
				method: 'POST',
				headers: {
					ctor: '::',
					_0: A2(_elm_lang$http$Http$header, 'Content-Type', 'application/json'),
					_1: {ctor: '[]'}
				},
				url: url,
				body: _user$project$Data_Request$emptyJsonBody,
				expect: _elm_lang$http$Http$expectStringResponse(_user$project$Data_Chain$decodeContractResponse),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _user$project$Request_Block$getPeers = function (nodeUrl) {
	var url = A2(_elm_lang$core$Basics_ops['++'], nodeUrl, '/network/peer_id');
	return A3(_elm_lang$http$Http$post, url, _user$project$Data_Request$emptyJsonBody, _user$project$Data_Chain$decodePeers);
};
var _user$project$Request_Block$getKeys = function (nodeUrl) {
	var url = A2(_elm_lang$core$Basics_ops['++'], nodeUrl, '/blocks/head/proto/context/keys');
	return A3(_elm_lang$http$Http$post, url, _user$project$Data_Request$emptyJsonBody, _user$project$Data_Chain$decodeKeys);
};
var _user$project$Request_Block$getContractIDs = function (nodeUrl) {
	var url = A2(_elm_lang$core$Basics_ops['++'], nodeUrl, '/blocks/head/proto/context/contracts');
	return A3(_elm_lang$http$Http$post, url, _user$project$Data_Request$emptyJsonBody, _user$project$Data_Chain$decodeContractIDs);
};
var _user$project$Request_Block$getChainStartingAt = F3(
	function (nodeUrl, length, blockhash) {
		var url = A2(_elm_lang$core$Basics_ops['++'], nodeUrl, '/blocks');
		var body = _elm_lang$http$Http$jsonBody(
			_elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'include_ops',
						_1: _elm_lang$core$Json_Encode$bool(true)
					},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'length',
							_1: _elm_lang$core$Json_Encode$int(length)
						},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'heads',
								_1: _elm_lang$core$Json_Encode$list(
									{
										ctor: '::',
										_0: _elm_lang$core$Json_Encode$string(blockhash),
										_1: {ctor: '[]'}
									})
							},
							_1: {ctor: '[]'}
						}
					}
				}));
		return A3(_elm_lang$http$Http$post, url, body, _user$project$Data_Chain$decodeBlocks);
	});
var _user$project$Request_Block$getHeads = function (nodeUrl) {
	var url = A2(_elm_lang$core$Basics_ops['++'], nodeUrl, '/blocks');
	return A3(_elm_lang$http$Http$post, url, _user$project$Data_Request$emptyJsonBody, _user$project$Data_Chain$decodeBlocks);
};

var _user$project$Request_Lib$decodeDebug = function (message) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		function (value) {
			var _p0 = A2(_elm_lang$core$Debug$log, message, value);
			return _elm_lang$core$Json_Decode$succeed(value);
		},
		_elm_lang$core$Json_Decode$value);
};

var _user$project$Request_Operation$decodeEndorsement = A2(
	_elm_lang$core$Json_Decode$andThen,
	function (kind) {
		var _p0 = kind;
		switch (_p0) {
			case 'endorsement':
				return A3(
					_elm_lang$core$Json_Decode$map2,
					_user$project$Data_Chain$Endorsement,
					A2(_elm_lang$core$Json_Decode$field, 'block', _elm_lang$core$Json_Decode$string),
					A2(_elm_lang$core$Json_Decode$field, 'slot', _elm_lang$core$Json_Decode$int));
			case 'seed_nonce_revelation':
				return A3(
					_elm_lang$core$Json_Decode$map2,
					_user$project$Data_Chain$SeedNonceRevelation,
					A2(_elm_lang$core$Json_Decode$field, 'level', _elm_lang$core$Json_Decode$int),
					A2(_elm_lang$core$Json_Decode$field, 'nonce', _elm_lang$core$Json_Decode$string));
			case 'transaction':
				return A3(
					_elm_lang$core$Json_Decode$map2,
					_user$project$Data_Chain$Transaction,
					A2(_elm_lang$core$Json_Decode$field, 'destination', _elm_lang$core$Json_Decode$string),
					A2(_elm_lang$core$Json_Decode$field, 'amount', _elm_lang$core$Json_Decode$int));
			case 'faucet':
				return A3(
					_elm_lang$core$Json_Decode$map2,
					_user$project$Data_Chain$Faucet,
					A2(_elm_lang$core$Json_Decode$field, 'id', _elm_lang$core$Json_Decode$string),
					A2(_elm_lang$core$Json_Decode$field, 'nonce', _elm_lang$core$Json_Decode$string));
			case 'delegation':
				return A2(
					_elm_lang$core$Json_Decode$map,
					_user$project$Data_Chain$Delegation,
					A2(_elm_lang$core$Json_Decode$field, 'delegate', _elm_lang$core$Json_Decode$string));
			default:
				return A2(
					_elm_lang$core$Json_Decode$map,
					_user$project$Data_Chain$Unknown,
					_user$project$Request_Lib$decodeDebug('unknown kind'));
		}
	},
	A2(_elm_lang$core$Json_Decode$field, 'kind', _elm_lang$core$Json_Decode$string));
var _user$project$Request_Operation$decodeSubOperation = _elm_lang$core$Json_Decode$oneOf(
	{
		ctor: '::',
		_0: _user$project$Request_Operation$decodeEndorsement,
		_1: {
			ctor: '::',
			_0: A2(_elm_lang$core$Json_Decode$map, _user$project$Data_Chain$Unknown, _elm_lang$core$Json_Decode$value),
			_1: {ctor: '[]'}
		}
	});
var _user$project$Request_Operation$decodeParsedOperation = A4(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional,
	'signature',
	A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _elm_lang$core$Json_Decode$string),
	_elm_lang$core$Maybe$Nothing,
	A4(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional,
		'source',
		A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _elm_lang$core$Json_Decode$string),
		_elm_lang$core$Maybe$Nothing,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'operations',
			_elm_lang$core$Json_Decode$list(_user$project$Request_Operation$decodeSubOperation),
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'branch',
				_elm_lang$core$Json_Decode$string,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
					'net_id',
					_elm_lang$core$Json_Decode$string,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'hash',
						_elm_lang$core$Json_Decode$string,
						_elm_lang$core$Json_Decode$succeed(_user$project$Data_Chain$ParsedOperation)))))));
var _user$project$Request_Operation$decodeBlockOperationDetails = A2(
	_elm_lang$core$Json_Decode$field,
	'ok',
	_elm_lang$core$Json_Decode$list(
		_elm_lang$core$Json_Decode$list(_user$project$Request_Operation$decodeParsedOperation)));
var _user$project$Request_Operation$getBlockOperations = F2(
	function (nodeUrl, blockHash) {
		var url = A2(
			_elm_lang$core$Basics_ops['++'],
			nodeUrl,
			A2(
				_elm_lang$core$Basics_ops['++'],
				'/blocks/',
				A2(_elm_lang$core$Basics_ops['++'], blockHash, '/proto/operations')));
		return A3(_elm_lang$http$Http$post, url, _user$project$Data_Request$emptyJsonBody, _user$project$Request_Operation$decodeBlockOperationDetails);
	});

var _user$project$Request$Heads = function (a) {
	return {ctor: 'Heads', _0: a};
};
var _user$project$Request$BlockOperations = F2(
	function (a, b) {
		return {ctor: 'BlockOperations', _0: a, _1: b};
	});
var _user$project$Request$getBlockOperationDetails = F2(
	function (model, blockHash) {
		return A2(_user$project$Data_Chain$blockNeedsOperations, model.chain, blockHash) ? A2(
			_elm_lang$http$Http$send,
			_elm_lang$core$Result$map(
				_user$project$Request$BlockOperations(blockHash)),
			A2(_user$project$Request_Operation$getBlockOperations, model.nodeUrl, blockHash)) : _elm_lang$core$Platform_Cmd$none;
	});
var _user$project$Request$Blocks = function (a) {
	return {ctor: 'Blocks', _0: a};
};
var _user$project$Request$getBranch = F3(
	function (desiredLength, model, blockhash) {
		var branchList = A2(_user$project$Data_Chain$getBranchList, model.chain, blockhash);
		var toGet = desiredLength - _elm_lang$core$List$length(branchList);
		if (_elm_lang$core$Native_Utils.cmp(toGet, 0) > 0) {
			var startHash = A2(
				_elm_lang$core$Maybe$withDefault,
				blockhash,
				A2(
					_elm_lang$core$Maybe$map,
					function (_) {
						return _.predecessor;
					},
					_elm_lang$core$List$head(
						_elm_lang$core$List$reverse(branchList))));
			return A2(
				_elm_lang$http$Http$send,
				_elm_lang$core$Result$map(_user$project$Request$Blocks),
				A3(_user$project$Request_Block$getChainStartingAt, model.nodeUrl, toGet, startHash));
		} else {
			return _elm_lang$core$Platform_Cmd$none;
		}
	});
var _user$project$Request$handleResponseData = F2(
	function (responseData, model) {
		var _p0 = responseData;
		switch (_p0.ctor) {
			case 'Heads':
				var newModel = _elm_lang$core$Native_Utils.update(
					model,
					{
						chain: A2(_user$project$Data_Chain$loadHeads, model.chain, _p0._0)
					});
				return {
					ctor: '_Tuple2',
					_0: newModel,
					_1: A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Platform_Cmd$none,
						A2(
							_elm_lang$core$Maybe$map,
							A2(_user$project$Request$getBranch, 24, model),
							_elm_lang$core$List$head(newModel.chain.heads)))
				};
			case 'Blocks':
				var getBlockOperations = function (blockHash) {
					return A2(
						_elm_lang$http$Http$send,
						_elm_lang$core$Result$map(
							_user$project$Request$BlockOperations(blockHash)),
						A2(_user$project$Request_Operation$getBlockOperations, model.nodeUrl, blockHash));
				};
				var newModel = _elm_lang$core$Native_Utils.update(
					model,
					{
						chain: A2(_user$project$Data_Chain$loadBlocks, model.chain, _p0._0)
					});
				var blocksToGet = _user$project$Data_Chain$blocksNeedingOperations(newModel.chain);
				return {
					ctor: '_Tuple2',
					_0: newModel,
					_1: _elm_lang$core$Platform_Cmd$batch(
						A2(_elm_lang$core$List$map, getBlockOperations, blocksToGet))
				};
			default:
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							chain: A3(_user$project$Data_Chain$addBlockOperations, model.chain, _p0._0, _p0._1)
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
		}
	});
var _user$project$Request$handleResponse = F2(
	function (response, model) {
		var _p1 = response;
		if (_p1.ctor === 'Ok') {
			var _p2 = A2(_user$project$Request$handleResponseData, _p1._0, model);
			var newModel = _p2._0;
			var cmd = _p2._1;
			return {ctor: '_Tuple3', _0: newModel, _1: cmd, _2: _elm_lang$core$Maybe$Nothing};
		} else {
			return {
				ctor: '_Tuple3',
				_0: model,
				_1: _elm_lang$core$Platform_Cmd$none,
				_2: _elm_lang$core$Maybe$Just(_p1._0)
			};
		}
	});

var _user$project$Request_Schema$getSchema = F2(
	function (nodeUrl, schemaQuery) {
		var url = A2(_elm_lang$core$Basics_ops['++'], nodeUrl, schemaQuery);
		var body = _elm_lang$http$Http$jsonBody(
			_elm_lang$core$Json_Encode$object(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'recursive',
						_1: _elm_lang$core$Json_Encode$bool(true)
					},
					_1: {ctor: '[]'}
				}));
		return A3(_elm_lang$http$Http$post, url, body, _user$project$Data_Schema$decodeSchema);
	});

var _user$project$Update$toPage = function (route) {
	var _p0 = route;
	switch (_p0.ctor) {
		case 'Home':
			return _user$project$Page$Home;
		case 'Block':
			return _user$project$Page$Block(_p0._0);
		case 'Operations':
			return _user$project$Page$Operations;
		case 'Operation':
			return _user$project$Page$Operation(_p0._0);
		case 'Heads':
			return _user$project$Page$Heads;
		case 'ChainAt':
			return _user$project$Page$ChainAt(_p0._0);
		case 'Contracts':
			return _user$project$Page$Contracts;
		case 'Keys':
			return _user$project$Page$Keys;
		case 'Peers':
			return _user$project$Page$Peers;
		case 'Contract':
			return _user$project$Page$Contract(_p0._0);
		case 'Schema':
			return _user$project$Page$Schema;
		case 'Errors':
			return _user$project$Page$Errors;
		case 'Debug':
			return _user$project$Page$Debug;
		default:
			return _user$project$Page$About;
	}
};
var _user$project$Update$addError = F2(
	function (error, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				errors: {ctor: '::', _0: error, _1: model.errors}
			});
	});
var _user$project$Update$addErrorMaybe = F2(
	function (errorMaybe, model) {
		var _p1 = errorMaybe;
		if (_p1.ctor === 'Just') {
			return A2(_user$project$Update$addError, _p1._0, model);
		} else {
			return model;
		}
	});
var _user$project$Update$RpcResponse = function (a) {
	return {ctor: 'RpcResponse', _0: a};
};
var _user$project$Update$getBlock = F2(
	function (model, hash) {
		var _p2 = A2(_elm_lang$core$Dict$get, hash, model.chain.blocks);
		if (_p2.ctor === 'Nothing') {
			return A2(
				_elm_lang$http$Http$send,
				function (_p3) {
					return _user$project$Update$RpcResponse(
						A2(_elm_lang$core$Result$map, _user$project$Request$Blocks, _p3));
				},
				A3(_user$project$Request_Block$getChainStartingAt, model.nodeUrl, 4, hash));
		} else {
			return _elm_lang$core$Platform_Cmd$none;
		}
	});
var _user$project$Update$getAllBlocksOperations = function (model) {
	var getBlockOperations = function (blockHash) {
		return A2(
			_elm_lang$http$Http$send,
			function (_p4) {
				return _user$project$Update$RpcResponse(
					A2(
						_elm_lang$core$Result$map,
						_user$project$Request$BlockOperations(blockHash),
						_p4));
			},
			A2(_user$project$Request_Operation$getBlockOperations, model.nodeUrl, blockHash));
	};
	var blocksToGet = _user$project$Data_Chain$blocksNeedingOperations(model.chain);
	return _elm_lang$core$Platform_Cmd$batch(
		A2(_elm_lang$core$List$map, getBlockOperations, blocksToGet));
};
var _user$project$Update$loadBlocks = F2(
	function (model, blocksData) {
		var newChain = A2(_user$project$Data_Chain$loadBlocks, model.chain, blocksData);
		var newModel = _elm_lang$core$Native_Utils.update(
			model,
			{chain: newChain});
		return {
			ctor: '_Tuple2',
			_0: newModel,
			_1: _user$project$Update$getAllBlocksOperations(newModel)
		};
	});
var _user$project$Update$updateMonitor = F2(
	function (data, model) {
		var blocksResult = A2(_elm_lang$core$Json_Decode$decodeValue, _user$project$Data_Chain$decodeBlocks, data);
		var newModel = function () {
			var _p5 = blocksResult;
			if (_p5.ctor === 'Ok') {
				return _elm_lang$core$Native_Utils.update(
					model,
					{
						chain: A2(_user$project$Data_Chain$updateMonitor, model.chain, _p5._0)
					});
			} else {
				return _elm_lang$core$Native_Utils.update(
					model,
					{
						errors: {
							ctor: '::',
							_0: _user$project$Model$OtherError(_p5._0),
							_1: model.errors
						}
					});
			}
		}();
		var blockHashes = A2(
			_elm_lang$core$Result$withDefault,
			{ctor: '[]'},
			A2(
				_elm_lang$core$Result$map,
				function (_p6) {
					return A2(
						_elm_lang$core$List$map,
						function (_) {
							return _.hash;
						},
						_elm_lang$core$List$concat(_p6));
				},
				blocksResult));
		var cmd = _elm_lang$core$Platform_Cmd$batch(
			A2(
				_elm_lang$core$List$map,
				function (_p7) {
					return A2(
						_elm_lang$core$Platform_Cmd$map,
						_user$project$Update$RpcResponse,
						A2(_user$project$Request$getBlockOperationDetails, model, _p7));
				},
				blockHashes));
		return {ctor: '_Tuple2', _0: newModel, _1: cmd};
	});
var _user$project$Update$Now = function (a) {
	return {ctor: 'Now', _0: a};
};
var _user$project$Update$Monitor = function (a) {
	return {ctor: 'Monitor', _0: a};
};
var _user$project$Update$ClearErrors = {ctor: 'ClearErrors'};
var _user$project$Update$SetRoute = function (a) {
	return {ctor: 'SetRoute', _0: a};
};
var _user$project$Update$Tick = function (a) {
	return {ctor: 'Tick', _0: a};
};
var _user$project$Update$LoadContract = F2(
	function (a, b) {
		return {ctor: 'LoadContract', _0: a, _1: b};
	});
var _user$project$Update$getContractDetails = F2(
	function (nodeUrl, contractIDs) {
		var get = function (contractId) {
			return A2(
				_elm_lang$http$Http$send,
				_user$project$Update$LoadContract(contractId),
				A2(_user$project$Request_Block$getContract, nodeUrl, contractId));
		};
		return _elm_lang$core$Platform_Cmd$batch(
			A2(_elm_lang$core$List$map, get, contractIDs));
	});
var _user$project$Update$LoadPeers = function (a) {
	return {ctor: 'LoadPeers', _0: a};
};
var _user$project$Update$getPeers = function (model) {
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Update$LoadPeers,
		_user$project$Request_Block$getPeers(model.nodeUrl));
};
var _user$project$Update$LoadKeys = function (a) {
	return {ctor: 'LoadKeys', _0: a};
};
var _user$project$Update$getKeys = function (model) {
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Update$LoadKeys,
		_user$project$Request_Block$getKeys(model.nodeUrl));
};
var _user$project$Update$LoadContractIDs = function (a) {
	return {ctor: 'LoadContractIDs', _0: a};
};
var _user$project$Update$getContractIDs = function (model) {
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Update$LoadContractIDs,
		_user$project$Request_Block$getContractIDs(model.nodeUrl));
};
var _user$project$Update$SchemaMsg = F2(
	function (a, b) {
		return {ctor: 'SchemaMsg', _0: a, _1: b};
	});
var _user$project$Update$LoadParsedOperation = F2(
	function (a, b) {
		return {ctor: 'LoadParsedOperation', _0: a, _1: b};
	});
var _user$project$Update$LoadSchema = F2(
	function (a, b) {
		return {ctor: 'LoadSchema', _0: a, _1: b};
	});
var _user$project$Update$setRoute = F2(
	function (routeMaybe, model) {
		var _p8 = routeMaybe;
		if (_p8.ctor === 'Nothing') {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{
						pageState: _user$project$Model$Loaded(_user$project$Page$NotFound)
					}),
				_1: _elm_lang$core$Platform_Cmd$none
			};
		} else {
			switch (_p8._0.ctor) {
				case 'Home':
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								pageState: _user$project$Model$Loaded(_user$project$Page$Home)
							}),
						_1: A2(
							_elm_lang$core$Maybe$withDefault,
							_elm_lang$core$Platform_Cmd$none,
							A2(
								_elm_lang$core$Maybe$map,
								function (_p9) {
									return A2(
										_elm_lang$core$Platform_Cmd$map,
										_user$project$Update$RpcResponse,
										A3(_user$project$Request$getBranch, 24, model, _p9));
								},
								_elm_lang$core$List$head(model.chain.heads)))
					};
				case 'Block':
					var _p10 = _p8._0._0;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								pageState: _user$project$Model$Loaded(
									_user$project$Page$Block(_p10))
							}),
						_1: _elm_lang$core$Platform_Cmd$batch(
							{
								ctor: '::',
								_0: A2(_user$project$Update$getBlock, model, _p10),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$core$Platform_Cmd$map,
										_user$project$Update$RpcResponse,
										A2(_user$project$Request$getBlockOperationDetails, model, _p10)),
									_1: {ctor: '[]'}
								}
							})
					};
				case 'ChainAt':
					var _p11 = _p8._0._0;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								pageState: _user$project$Model$Loaded(
									_user$project$Page$ChainAt(_p11))
							}),
						_1: A2(
							_elm_lang$core$Platform_Cmd$map,
							_user$project$Update$RpcResponse,
							A3(_user$project$Request$getBranch, 24, model, _p11))
					};
				case 'Contracts':
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								pageState: _user$project$Model$Loaded(_user$project$Page$Contracts),
								chain: _user$project$Data_Chain$loadingContractIDs(model.chain)
							}),
						_1: _user$project$Update$getContractIDs(model)
					};
				case 'Keys':
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								pageState: _user$project$Model$Loaded(_user$project$Page$Keys),
								chain: _user$project$Data_Chain$loadingKeys(model.chain)
							}),
						_1: _user$project$Update$getKeys(model)
					};
				case 'Peers':
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								pageState: _user$project$Model$Loaded(_user$project$Page$Peers),
								chain: _user$project$Data_Chain$loadingPeers(model.chain)
							}),
						_1: _user$project$Update$getPeers(model)
					};
				case 'Contract':
					var _p12 = _p8._0._0;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								pageState: _user$project$Model$Loaded(
									_user$project$Page$Contract(_p12)),
								chain: A2(_user$project$Data_Chain$loadingContract, model.chain, _p12)
							}),
						_1: A2(
							_elm_lang$http$Http$send,
							_user$project$Update$LoadContract(_p12),
							A2(_user$project$Request_Block$getContract, model.nodeUrl, _p12))
					};
				case 'Schema':
					var schemaQuery2 = '/describe/blocks/head/proto';
					var schemaQuery1 = '/describe';
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								pageState: _user$project$Model$Loaded(_user$project$Page$Schema)
							}),
						_1: _elm_lang$core$Platform_Cmd$batch(
							{
								ctor: '::',
								_0: A2(
									_elm_lang$http$Http$send,
									_user$project$Update$LoadSchema(schemaQuery1),
									A2(_user$project$Request_Schema$getSchema, model.nodeUrl, schemaQuery1)),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$http$Http$send,
										_user$project$Update$LoadSchema(schemaQuery2),
										A2(_user$project$Request_Schema$getSchema, model.nodeUrl, schemaQuery2)),
									_1: {ctor: '[]'}
								}
							})
					};
				default:
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								pageState: _user$project$Model$Loaded(
									_user$project$Update$toPage(_p8._0))
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
			}
		}
	});
var _user$project$Update$updatePage = F3(
	function (page, msg, model) {
		var _p13 = {ctor: '_Tuple2', _0: msg, _1: page};
		switch (_p13._0.ctor) {
			case 'RpcResponse':
				var _p14 = A2(_user$project$Request$handleResponse, _p13._0._0, model);
				var newModel = _p14._0;
				var cmd = _p14._1;
				var errorMaybe = _p14._2;
				return {
					ctor: '_Tuple2',
					_0: A2(
						_user$project$Update$addErrorMaybe,
						A2(_elm_lang$core$Maybe$map, _user$project$Model$HttpError, errorMaybe),
						newModel),
					_1: A2(_elm_lang$core$Platform_Cmd$map, _user$project$Update$RpcResponse, cmd)
				};
			case 'LoadSchema':
				var _p15 = _p13._0._1;
				if (_p15.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								schemaData: A3(
									_elm_lang$core$Dict$insert,
									_p13._0._0,
									_user$project$Data_Schema$collapseTrees(_p15._0),
									model.schemaData)
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								errors: {
									ctor: '::',
									_0: _user$project$Model$HttpError(_p15._0),
									_1: model.errors
								}
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				}
			case 'SchemaMsg':
				var _p18 = _p13._0._0;
				var newSchemaMaybe = A2(
					_elm_lang$core$Maybe$map,
					_user$project$Data_Schema$update(_p13._0._1),
					A2(_elm_lang$core$Dict$get, _p18, model.schemaData));
				var _p16 = newSchemaMaybe;
				if (_p16.ctor === 'Just') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								schemaData: A3(_elm_lang$core$Dict$insert, _p18, _p16._0, model.schemaData)
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					var _p17 = A2(_elm_lang$core$Debug$log, 'Failed to find schema', _p18);
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
			case 'LoadParsedOperation':
				var _p19 = _p13._0._1;
				if (_p19.ctor === 'Ok') {
					var newChain = A3(_user$project$Data_Chain$loadParsedOperation, model.chain, _p13._0._0, _p19._0);
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{chain: newChain}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								errors: {
									ctor: '::',
									_0: _user$project$Model$HttpError(_p19._0),
									_1: model.errors
								}
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				}
			case 'LoadContractIDs':
				var _p20 = _p13._0._0;
				if (_p20.ctor === 'Ok') {
					var _p21 = _p20._0;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								chain: A2(_user$project$Data_Chain$loadContractIDs, model.chain, _p21)
							}),
						_1: A2(_user$project$Update$getContractDetails, model.nodeUrl, _p21)
					};
				} else {
					var _p22 = _p20._0;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								errors: {
									ctor: '::',
									_0: _user$project$Model$HttpError(_p22),
									_1: model.errors
								},
								chain: A2(_user$project$Data_Chain$loadContractIDsError, model.chain, _p22)
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				}
			case 'LoadKeys':
				var _p23 = _p13._0._0;
				if (_p23.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								chain: A2(_user$project$Data_Chain$loadKeys, model.chain, _p23._0)
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					var _p24 = _p23._0;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								errors: {
									ctor: '::',
									_0: _user$project$Model$HttpError(_p24),
									_1: model.errors
								},
								chain: A2(_user$project$Data_Chain$loadKeysError, model.chain, _p24)
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				}
			case 'LoadPeers':
				var _p25 = _p13._0._0;
				if (_p25.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								chain: A2(_user$project$Data_Chain$loadPeers, model.chain, _p25._0)
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					var _p26 = _p25._0;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								errors: {
									ctor: '::',
									_0: _user$project$Model$HttpError(_p26),
									_1: model.errors
								},
								chain: A2(_user$project$Data_Chain$loadPeersError, model.chain, _p26)
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				}
			case 'LoadContract':
				var _p29 = _p13._0._0;
				var _p27 = _p13._0._1;
				if (_p27.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								chain: A3(_user$project$Data_Chain$loadContract, model.chain, _p29, _p27._0)
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					var _p28 = _p27._0;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								errors: {
									ctor: '::',
									_0: _user$project$Model$HttpError(_p28),
									_1: model.errors
								},
								chain: A3(_user$project$Data_Chain$loadContractError, model.chain, _p29, _p28)
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				}
			case 'Tick':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							now: _elm_lang$core$Date$fromTime(_p13._0._0)
						}),
					_1: A2(
						_elm_lang$http$Http$send,
						function (_p30) {
							return _user$project$Update$RpcResponse(
								A2(_elm_lang$core$Result$map, _user$project$Request$Heads, _p30));
						},
						_user$project$Request_Block$getHeads(model.nodeUrl))
				};
			case 'Now':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							now: _elm_lang$core$Date$fromTime(_p13._0._0)
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'SetRoute':
				return A2(_user$project$Update$setRoute, _p13._0._0, model);
			case 'ClearErrors':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							errors: {ctor: '[]'}
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			default:
				var _p31 = A2(_user$project$Update$updateMonitor, _p13._0._0, model);
				var newModel = _p31._0;
				var cmd = _p31._1;
				return {
					ctor: '_Tuple2',
					_0: newModel,
					_1: _elm_lang$core$Platform_Cmd$batch(
						{
							ctor: '::',
							_0: cmd,
							_1: {
								ctor: '::',
								_0: A2(_elm_lang$core$Task$perform, _user$project$Update$Now, _elm_lang$core$Time$now),
								_1: {ctor: '[]'}
							}
						})
				};
		}
	});
var _user$project$Update$update = F2(
	function (msg, model) {
		return A3(
			_user$project$Update$updatePage,
			_user$project$Model$getPage(model.pageState),
			msg,
			model);
	});

var _user$project$View_Page$viewErrorCount = function (count) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$classList(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'error-count', _1: true},
					_1: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'have-errors',
							_1: _elm_lang$core$Native_Utils.cmp(count, 0) > 0
						},
						_1: {ctor: '[]'}
					}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$a,
				{
					ctor: '::',
					_0: _user$project$Route$href(_user$project$Route$Errors),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(
						_elm_lang$core$Basics$toString(count)),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _user$project$View_Page$isRouteForPage = F2(
	function (route, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Utils.eq(
			_user$project$Update$toPage(route),
			_p1._0);
	});
var _user$project$View_Page$links = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: 'Home', _1: _user$project$Route$Home},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'Heads', _1: _user$project$Route$Heads},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'Operations', _1: _user$project$Route$Operations},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'Contracts', _1: _user$project$Route$Contracts},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'Keys', _1: _user$project$Route$Keys},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'Peers', _1: _user$project$Route$Peers},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'Schemas', _1: _user$project$Route$Schema},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'About', _1: _user$project$Route$About},
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}
		}
	}
};
var _user$project$View_Page$navLinks = function (pageState) {
	var makeNavLink = function (_p2) {
		var _p3 = _p2;
		var _p4 = _p3._1;
		return A2(
			_elm_lang$html$Html$li,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'active',
							_1: A2(_user$project$View_Page$isRouteForPage, _p4, pageState)
						},
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$a,
					{
						ctor: '::',
						_0: _user$project$Route$href(_p4),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(_p3._0),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	};
	return A2(
		_elm_lang$html$Html$ul,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('nav-links'),
			_1: {ctor: '[]'}
		},
		A2(_elm_lang$core$List$map, makeNavLink, _user$project$View_Page$links));
};
var _user$project$View_Page$formatDate = _rluiten$elm_date_extra$Date_Extra_Format$format(_rluiten$elm_date_extra$Date_Extra_Config_Config_en_us$config);
var _user$project$View_Page$viewNow = function (now) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('now'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$title(
					A2(_user$project$View_Page$formatDate, '%Y/%m/%d %H:%M:%S', now)),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(
				A2(_user$project$View_Page$formatDate, '%H:%M', now)),
			_1: {ctor: '[]'}
		});
};
var _user$project$View_Page$viewHeader = function (context) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('page-header'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h1,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Tezos Explorer'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: _user$project$View_Page$viewNow(context.now),
				_1: {
					ctor: '::',
					_0: _user$project$View_Page$viewErrorCount(context.errorCount),
					_1: {
						ctor: '::',
						_0: _user$project$View_Page$navLinks(context.pageState),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _user$project$View_Page$frame = F2(
	function (context, content) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('page-frame'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: _user$project$View_Page$viewHeader(context),
				_1: {
					ctor: '::',
					_0: content,
					_1: {ctor: '[]'}
				}
			});
	});
var _user$project$View_Page$Context = F3(
	function (a, b, c) {
		return {pageState: a, now: b, errorCount: c};
	});

var _user$project$View$viewAbout = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h3,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('About'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$p,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('This Tezos blockchain explorer displays data from the Tezos '),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$i,
								{ctor: '[]'},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('alphanet'),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html$text(', the testnet for Tezos.'),
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$p,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('The data comes via the Tezos RPC over HTTPS from a single node on the network ('),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html$text(model.nodeUrl),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html$text(') and therefore depends on the status of that node. Being a test network, sometimes that node may be out of service or out of sync with the current alphanet chain. '),
									_1: {ctor: '[]'}
								}
							}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$p,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('This Tezos Explorer client was developed by Fred Yankowski (@fredcy) and is under '),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$a,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$href('https://github.com/fredcy/tezos-client'),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text('active development. '),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html$text('Suggestions for additions or changes to this client are welcome. '),
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _user$project$View$viewDebug = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('debug'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h2,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Raw model'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html$text(
					_elm_lang$core$Basics$toString(model)),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$viewErrorInfo = F2(
	function (nodeUrl, error) {
		var _p0 = error;
		_v0_3:
		do {
			if (_p0.ctor === 'HttpError') {
				switch (_p0._0.ctor) {
					case 'BadPayload':
						return A2(
							_elm_lang$html$Html$div,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$h4,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('Bad Payload (JSON parsing problem)'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$div,
										{ctor: '[]'},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text(_p0._0._0),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$h5,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: _elm_lang$html$Html$text('Response body'),
												_1: {ctor: '[]'}
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_lang$html$Html$pre,
												{ctor: '[]'},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text(_p0._0._1.body),
													_1: {ctor: '[]'}
												}),
											_1: {ctor: '[]'}
										}
									}
								}
							});
					case 'BadStatus':
						var _p1 = _p0._0._0;
						return A2(
							_elm_lang$html$Html$div,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$h4,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('Bad response status from node'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$div,
										{ctor: '[]'},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text(
												_elm_lang$core$Basics$toString(_p1.status)),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$div,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: _elm_lang$html$Html$text(_p1.url),
												_1: {ctor: '[]'}
											}),
										_1: {ctor: '[]'}
									}
								}
							});
					case 'NetworkError':
						return A2(
							_elm_lang$html$Html$div,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$h4,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('Network Error'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$div,
										{ctor: '[]'},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text(
												A2(_elm_lang$core$Basics_ops['++'], 'Unable to access Tezos node at ', nodeUrl)),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}
							});
					default:
						break _v0_3;
				}
			} else {
				break _v0_3;
			}
		} while(false);
		return _elm_lang$html$Html$text(
			_elm_lang$core$Basics$toString(error));
	});
var _user$project$View$viewError = F2(
	function (nodeUrl, errors) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('error'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$h1,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('Errors'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$button,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onClick(_user$project$Update$ClearErrors),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('Clear all errors'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{ctor: '[]'},
							A2(
								_elm_lang$core$List$map,
								_user$project$View$viewErrorInfo(nodeUrl),
								errors)),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _user$project$View$viewResponseBody = function (body) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'white-space', _1: 'pre'},
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(body),
			_1: {ctor: '[]'}
		});
};
var _user$project$View$viewFailure = function (error) {
	var _p2 = error;
	if (_p2.ctor === 'BadPayload') {
		var _p3 = _p2._1;
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$h4,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('Error: Bad Payload'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(_p2._0),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$h5,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('Response'),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$div,
								{ctor: '[]'},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(
										_elm_lang$core$Basics$toString(_p3)),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$h5,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('Body'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: _user$project$View$viewResponseBody(_p3.body),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			});
	} else {
		return _elm_lang$html$Html$text(
			_elm_lang$core$Basics$toString(error));
	}
};
var _user$project$View$hoistSingletonLists = function (program) {
	hoistSingletonLists:
	while (true) {
		var _p4 = program;
		switch (_p4.ctor) {
			case 'SeqT':
				if ((_p4._0.ctor === '::') && (_p4._0._1.ctor === '[]')) {
					var _v3 = _p4._0._0;
					program = _v3;
					continue hoistSingletonLists;
				} else {
					return _user$project$Data_Michelson$SeqT(
						A2(_elm_lang$core$List$map, _user$project$View$hoistSingletonLists, _p4._0));
				}
			case 'PrimArgT':
				return A2(
					_user$project$Data_Michelson$PrimArgT,
					_p4._0,
					_user$project$View$hoistSingletonLists(_p4._1));
			default:
				return program;
		}
	}
};
var _user$project$View$combinePrimitives = function (program) {
	var cxrLetters = function (prim) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			_elm_lang$core$Maybe$Nothing,
			A2(
				_elm_lang$core$Maybe$andThen,
				_elm_lang$core$List$head,
				A2(
					_elm_lang$core$Maybe$map,
					function (_) {
						return _.submatches;
					},
					_elm_lang$core$List$head(
						A3(
							_elm_lang$core$Regex$find,
							_elm_lang$core$Regex$AtMost(1),
							_elm_lang$core$Regex$regex('^C([AD])+R$'),
							prim)))));
	};
	var combinePrims = F2(
		function (prim1, prim2) {
			return A2(
				_elm_lang$core$Maybe$andThen,
				function (letters1) {
					return A2(
						_elm_lang$core$Maybe$andThen,
						function (letters2) {
							return _elm_lang$core$Maybe$Just(
								A2(
									_elm_lang$core$Basics_ops['++'],
									'C',
									A2(
										_elm_lang$core$Basics_ops['++'],
										letters1,
										A2(_elm_lang$core$Basics_ops['++'], letters2, 'R'))));
						},
						cxrLetters(prim2));
				},
				cxrLetters(prim1));
		});
	var simplifySeq = F2(
		function (item, sofar) {
			var _p5 = {ctor: '_Tuple2', _0: item, _1: sofar};
			if ((((_p5.ctor === '_Tuple2') && (_p5._0.ctor === 'PrimT')) && (_p5._1.ctor === '::')) && (_p5._1._0.ctor === 'PrimT')) {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					{ctor: '::', _0: item, _1: sofar},
					A2(
						_elm_lang$core$Maybe$map,
						function (newprim) {
							return {
								ctor: '::',
								_0: _user$project$Data_Michelson$PrimT(newprim),
								_1: _p5._1._1
							};
						},
						A2(combinePrims, _p5._0._0, _p5._1._0._0)));
			} else {
				return {
					ctor: '::',
					_0: _user$project$View$combinePrimitives(item),
					_1: sofar
				};
			}
		});
	var _p6 = program;
	switch (_p6.ctor) {
		case 'SeqT':
			return _user$project$Data_Michelson$SeqT(
				A3(
					_elm_lang$core$List$foldr,
					simplifySeq,
					{ctor: '[]'},
					_p6._0));
		case 'PrimArgT':
			return A2(
				_user$project$Data_Michelson$PrimArgT,
				_p6._0,
				_user$project$View$combinePrimitives(_p6._1));
		default:
			return program;
	}
};
var _user$project$View$simplifyProgram1 = function (program) {
	return _user$project$View$hoistSingletonLists(
		_user$project$View$combinePrimitives(program));
};
var _user$project$View$primCss = function (prim) {
	return A2(
		_elm_lang$core$String$map,
		function (c) {
			return _elm_lang$core$Native_Utils.eq(
				c,
				_elm_lang$core$Native_Utils.chr('_')) ? _elm_lang$core$Native_Utils.chr('-') : c;
		},
		prim);
};
var _user$project$View$viewProgram = function (program) {
	var _p7 = program;
	switch (_p7.ctor) {
		case 'IntT':
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('IntT'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(
						_elm_lang$core$Basics$toString(_p7._0)),
					_1: {ctor: '[]'}
				});
		case 'StringT':
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('StringT'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'\"',
							A2(_elm_lang$core$Basics_ops['++'], _p7._0, '\"'))),
					_1: {ctor: '[]'}
				});
		case 'PrimT':
			var _p8 = _p7._0;
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'PrimT prim-',
							_user$project$View$primCss(_p8))),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(_p8),
					_1: {ctor: '[]'}
				});
		case 'SeqT':
			return A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('SeqT'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(' { '),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('sequence'),
								_1: {ctor: '[]'}
							},
							A2(_elm_lang$core$List$map, _user$project$View$viewProgram, _p7._0)),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html$text(' } '),
							_1: {ctor: '[]'}
						}
					}
				});
		default:
			var _p9 = _p7._0;
			return A2(
				_elm_lang$html$Html$span,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'PrimArgT primarg primarg-',
							_user$project$View$primCss(_p9))),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class(
								A2(
									_elm_lang$core$Basics_ops['++'],
									'PrimT prim-',
									_user$project$View$primCss(_p9))),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(_p9),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class(
									A2(
										_elm_lang$core$Basics_ops['++'],
										'primargarg primargarg-',
										_user$project$View$primCss(_p9))),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _user$project$View$viewProgram(_p7._1),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				});
	}
};
var _user$project$View$simplifyHost = function (host) {
	var prefix = '::ffff:';
	return A2(_elm_lang$core$String$startsWith, prefix, host) ? A2(
		_elm_lang$core$String$dropLeft,
		_elm_lang$core$String$length(prefix),
		host) : host;
};
var _user$project$View$viewAddress = function (address) {
	return A2(
		_elm_lang$html$Html$span,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(
				_user$project$View$simplifyHost(address.addr)),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html$text(':'),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html$text(
						_elm_lang$core$Basics$toString(address.port_)),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$View$viewKeyList = function (keys) {
	var tableRow = function (key) {
		return A2(
			_elm_lang$html$Html$tr,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$td,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('hash'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(key.hash),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$td,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('hash'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(key.public_key),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	};
	var tableBody = function (keys) {
		return A2(
			_elm_lang$html$Html$tbody,
			{ctor: '[]'},
			A2(_elm_lang$core$List$map, tableRow, keys));
	};
	var tableHead = A2(
		_elm_lang$html$Html$thead,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$tr,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$th,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('hash'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$th,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('public_key'),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				}),
			_1: {ctor: '[]'}
		});
	return A2(
		_elm_lang$html$Html$table,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('keys'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: tableHead,
			_1: {
				ctor: '::',
				_0: tableBody(keys),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$viewKeys = function (keysData) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h3,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Keys '),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: function () {
					var _p10 = keysData;
					switch (_p10.ctor) {
						case 'Success':
							return _user$project$View$viewKeyList(_p10._0);
						case 'Loading':
							return _elm_lang$html$Html$text('loading ...');
						default:
							return _elm_lang$html$Html$text(
								_elm_lang$core$Basics$toString(keysData));
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$shortHash = function (hash) {
	return A2(_elm_lang$core$String$left, 14, hash);
};
var _user$project$View$formatCentiles = function (number) {
	var usLocale = _cuducos$elm_format_number$FormatNumber_Locales$usLocale;
	return A2(
		_cuducos$elm_format_number$FormatNumber$format,
		_elm_lang$core$Native_Utils.update(
			usLocale,
			{decimals: 2}),
		_elm_lang$core$Basics$toFloat(number) / 100);
};
var _user$project$View$viewSuboperation = function (suboperation) {
	var _p11 = suboperation;
	switch (_p11.ctor) {
		case 'Endorsement':
			return A2(
				_elm_lang$html$Html$span,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Endorsement of '),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('hash'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(
									_user$project$View$shortHash(_p11._0)),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html$text(
								A2(
									_elm_lang$core$Basics_ops['++'],
									', slot ',
									_elm_lang$core$Basics$toString(_p11._1))),
							_1: {ctor: '[]'}
						}
					}
				});
		case 'Transaction':
			var _p12 = _p11._0;
			return A2(
				_elm_lang$html$Html$span,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Transaction of '),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_user$project$View$formatCentiles(_p11._1)),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html$text(' ꜩ to '),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$span,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$title(_p12),
										_1: {
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('hash'),
											_1: {ctor: '[]'}
										}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											_user$project$View$shortHash(_p12)),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						}
					}
				});
		case 'Delegation':
			var _p13 = _p11._0;
			return A2(
				_elm_lang$html$Html$span,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Delegation to '),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('hash'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$a,
									{
										ctor: '::',
										_0: _user$project$Route$href(
											_user$project$Route$Contract(_p13)),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											_user$project$View$shortHash(_p13)),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				});
		default:
			return _elm_lang$html$Html$text(
				_elm_lang$core$Basics$toString(suboperation));
	}
};
var _user$project$View$viewOperationsTable = function (operations) {
	var sourceTitle = function (sourceMaybe) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(_elm_lang$core$Maybe$map, _elm_lang$core$Basics$identity, sourceMaybe));
	};
	var viewSourceMaybe = function (sourceMaybe) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'[no source]',
			A2(_elm_lang$core$Maybe$map, _user$project$View$shortHash, sourceMaybe));
	};
	var tableRow = function (operation) {
		return A2(
			_elm_lang$html$Html$tr,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$td,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('hash'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$a,
							{
								ctor: '::',
								_0: _user$project$Route$href(
									_user$project$Route$Operation(operation.hash)),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$title(operation.hash),
									_1: {ctor: '[]'}
								}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(
									_user$project$View$shortHash(operation.hash)),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$td,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('hash'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$title(
									sourceTitle(operation.source)),
								_1: {ctor: '[]'}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(
								viewSourceMaybe(operation.source)),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$td,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$ul,
									{ctor: '[]'},
									A2(
										_elm_lang$core$List$map,
										function (so) {
											return A2(
												_elm_lang$html$Html$li,
												{ctor: '[]'},
												{
													ctor: '::',
													_0: _user$project$View$viewSuboperation(so),
													_1: {ctor: '[]'}
												});
										},
										operation.operations)),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				}
			});
	};
	var tableHead = A2(
		_elm_lang$html$Html$thead,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$tr,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$th,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('hash'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$th,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('source'),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$th,
								{ctor: '[]'},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('sub-operations'),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}
				}),
			_1: {ctor: '[]'}
		});
	return A2(
		_elm_lang$html$Html$table,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('operations'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: tableHead,
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$tbody,
					{ctor: '[]'},
					A2(_elm_lang$core$List$map, tableRow, operations)),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$viewOperations = function (operations) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h3,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Operations'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: _user$project$View$viewOperationsTable(operations),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$viewAllOperations = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h3,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('All Operations'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$p,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('(This displays only those operations that we have seen in the most\n                           recent blocks. This will be improved soon.)'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: _user$project$View$viewOperationsTable(
						A2(
							_elm_lang$core$List$sortBy,
							function (_) {
								return _.hash;
							},
							A2(
								_elm_lang$core$List$map,
								_elm_lang$core$Tuple$second,
								_elm_lang$core$Dict$toList(model.chain.parsedOperations)))),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$View$viewContractTable = F2(
	function (contractIDs, contracts) {
		var trow = function (contractId) {
			var contractData = A2(
				_elm_lang$core$Maybe$withDefault,
				_krisajenkins$remotedata$RemoteData$NotAsked,
				A2(_elm_lang$core$Dict$get, contractId, contracts));
			var _p14 = contractData;
			switch (_p14.ctor) {
				case 'Success':
					var _p16 = _p14._0;
					return A2(
						_elm_lang$html$Html$tr,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$td,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('hash'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$a,
										{
											ctor: '::',
											_0: _user$project$Route$href(
												_user$project$Route$Contract(contractId)),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text(
												_user$project$View$shortHash(contractId)),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$td,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('balance'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											_user$project$View$formatCentiles(_p16.balance)),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$td,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('hash'),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: A2(
												_elm_lang$html$Html$a,
												{
													ctor: '::',
													_0: _user$project$Route$href(
														_user$project$Route$Contract(_p16.manager)),
													_1: {ctor: '[]'}
												},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text(
														_user$project$View$shortHash(_p16.manager)),
													_1: {ctor: '[]'}
												}),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$td,
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$class('number'),
												_1: {ctor: '[]'}
											},
											{
												ctor: '::',
												_0: _elm_lang$html$Html$text(
													_elm_lang$core$Basics$toString(_p16.counter)),
												_1: {ctor: '[]'}
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_lang$html$Html$td,
												{
													ctor: '::',
													_0: _elm_lang$html$Html_Attributes$class('number'),
													_1: {ctor: '[]'}
												},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text(
														A2(
															_elm_lang$core$Maybe$withDefault,
															'',
															A2(
																_elm_lang$core$Maybe$map,
																function (_p15) {
																	return _elm_lang$core$Basics$toString(
																		_elm_lang$core$String$length(
																			_elm_lang$core$Basics$toString(_p15)));
																},
																_p16.script))),
													_1: {ctor: '[]'}
												}),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						});
				case 'NotAsked':
					return A2(
						_elm_lang$html$Html$tr,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$td,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('hash link'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(
										_user$project$View$shortHash(contractId)),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$td,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('...'),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						});
				case 'Loading':
					return A2(
						_elm_lang$html$Html$tr,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$td,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('hash link'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(
										_user$project$View$shortHash(contractId)),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$td,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('......'),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						});
				default:
					return A2(
						_elm_lang$html$Html$tr,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$td,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('hash link'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(
										_user$project$View$shortHash(contractId)),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$td,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											_elm_lang$core$Basics$toString(_p14._0)),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						});
			}
		};
		var tbody = A2(
			_elm_lang$html$Html$tbody,
			{ctor: '[]'},
			A2(
				_elm_lang$core$List$map,
				trow,
				_elm_lang$core$List$sort(contractIDs)));
		var thead = A2(
			_elm_lang$html$Html$tr,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$th,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('contractID'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$th,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('balance (ꜩ)'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$th,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('manager'),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$th,
								{ctor: '[]'},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('counter'),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$th,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('script size'),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			});
		return A2(
			_elm_lang$html$Html$table,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('contracts'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: thead,
				_1: {
					ctor: '::',
					_0: tbody,
					_1: {ctor: '[]'}
				}
			});
	});
var _user$project$View$viewContracts = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h3,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Contracts'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: function () {
					var _p17 = model.chain.contractIDs;
					switch (_p17.ctor) {
						case 'Success':
							return A2(_user$project$View$viewContractTable, _p17._0, model.chain.contracts);
						case 'Loading':
							return _elm_lang$html$Html$text('loading ...');
						default:
							return _elm_lang$html$Html$text(
								_elm_lang$core$Basics$toString(model.chain.contractIDs));
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$formatInt = function (number) {
	var usLocale = _cuducos$elm_format_number$FormatNumber_Locales$usLocale;
	return A2(
		_cuducos$elm_format_number$FormatNumber$format,
		_elm_lang$core$Native_Utils.update(
			usLocale,
			{decimals: 0}),
		_elm_lang$core$Basics$toFloat(number));
};
var _user$project$View$viewShowBlockOperations = F2(
	function (blockOperations, hashMaybe) {
		var _p18 = hashMaybe;
		if (_p18.ctor === 'Just') {
			return A2(
				_elm_lang$core$Maybe$withDefault,
				_elm_lang$html$Html$text(''),
				A2(
					_elm_lang$core$Maybe$map,
					_user$project$View$viewOperations,
					A2(_elm_lang$core$Dict$get, _p18._0, blockOperations)));
		} else {
			return _elm_lang$html$Html$text('');
		}
	});
var _user$project$View$formatDate = function (date) {
	return A3(_rluiten$elm_date_extra$Date_Extra_Format$formatUtc, _rluiten$elm_date_extra$Date_Extra_Config_Config_en_us$config, '%Y-%m-%dT%H:%M:%SZ', date);
};
var _user$project$View$viewConnection = function (connectionM) {
	var _p19 = connectionM;
	if (_p19.ctor === 'Just') {
		var _p20 = _p19._0;
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('connection'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: _user$project$View$viewAddress(_p20.addr),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html$text(' '),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('timestamp'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(
									_user$project$View$formatDate(_p20.time)),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				}
			});
	} else {
		return _elm_lang$html$Html$text('');
	}
};
var _user$project$View$viewPeersList = function (peers) {
	var colHeader = function (label) {
		return A2(
			_elm_lang$html$Html$th,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(label),
				_1: {ctor: '[]'}
			});
	};
	var thead = A2(
		_elm_lang$html$Html$thead,
		{ctor: '[]'},
		A2(
			_elm_lang$core$List$map,
			colHeader,
			{
				ctor: '::',
				_0: 'id',
				_1: {
					ctor: '::',
					_0: 'state',
					_1: {
						ctor: '::',
						_0: 'trusted',
						_1: {
							ctor: '::',
							_0: 'score',
							_1: {
								ctor: '::',
								_0: 'sent',
								_1: {
									ctor: '::',
									_0: 'recv',
									_1: {
										ctor: '::',
										_0: 'last connection',
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			}));
	var row = function (peer) {
		return A2(
			_elm_lang$html$Html$tr,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$td,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('hash'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_user$project$View$shortHash(peer.hash)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$td,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(peer.info.state),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$td,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(
									_elm_lang$core$Basics$toString(peer.info.trusted)),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$td,
								{ctor: '[]'},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(
										_elm_lang$core$Basics$toString(peer.info.score)),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$td,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('number'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											_elm_lang$core$Basics$toString(peer.info.stat.total_sent)),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$td,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('number'),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text(
												_elm_lang$core$Basics$toString(peer.info.stat.total_recv)),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$td,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: _user$project$View$viewConnection(peer.info.lastConnection),
												_1: {ctor: '[]'}
											}),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			});
	};
	var tbody = A2(
		_elm_lang$html$Html$tbody,
		{ctor: '[]'},
		A2(_elm_lang$core$List$map, row, peers));
	return A2(
		_elm_lang$html$Html$table,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('peers'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: thead,
			_1: {
				ctor: '::',
				_0: tbody,
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$viewPeers = function (peersData) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h3,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Peers'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: function () {
					var _p21 = peersData;
					if (_p21.ctor === 'Success') {
						return _user$project$View$viewPeersList(_p21._0);
					} else {
						return _elm_lang$html$Html$text(
							_elm_lang$core$Basics$toString(peersData));
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$viewParsedOperations = F2(
	function (model, blockId) {
		var operationIDs = A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '[]'},
			A2(_elm_lang$core$Dict$get, blockId, model.chain.blockOperations));
		var operations = A2(
			_elm_lang$core$List$filterMap,
			_elm_lang$core$Basics$identity,
			A2(
				_elm_lang$core$List$map,
				function (oid) {
					return A2(_elm_lang$core$Dict$get, oid, model.chain.parsedOperations);
				},
				operationIDs));
		return _user$project$View$viewOperationsTable(operations);
	});
var _user$project$View$viewProperty = F2(
	function (label, value) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('property'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('label'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(label),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class(label),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: value,
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _user$project$View$viewPropertyMaybe = F2(
	function (label, valueMaybe) {
		var _p22 = valueMaybe;
		if (_p22.ctor === 'Just') {
			return A2(
				_user$project$View$viewProperty,
				label,
				_elm_lang$html$Html$text(_p22._0));
		} else {
			return _elm_lang$html$Html$text('');
		}
	});
var _user$project$View$viewOperation = F2(
	function (model, operationId) {
		var viewOperationFields = function (operation) {
			return A2(
				_elm_lang$html$Html$div,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_user$project$View$viewProperty,
						'hash',
						_elm_lang$html$Html$text(operation.hash)),
					_1: {
						ctor: '::',
						_0: A2(
							_user$project$View$viewProperty,
							'net_id',
							_elm_lang$html$Html$text(operation.net_id)),
						_1: {
							ctor: '::',
							_0: A2(
								_user$project$View$viewProperty,
								'branch',
								_elm_lang$html$Html$text(operation.branch)),
							_1: {
								ctor: '::',
								_0: A2(_user$project$View$viewPropertyMaybe, 'source', operation.source),
								_1: {
									ctor: '::',
									_0: A2(_user$project$View$viewPropertyMaybe, 'signature', operation.signature),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$h4,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: _elm_lang$html$Html$text('operations'),
												_1: {ctor: '[]'}
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_lang$html$Html$ul,
												{ctor: '[]'},
												A2(
													_elm_lang$core$List$map,
													function (so) {
														return A2(
															_elm_lang$html$Html$li,
															{ctor: '[]'},
															{
																ctor: '::',
																_0: _user$project$View$viewSuboperation(so),
																_1: {ctor: '[]'}
															});
													},
													operation.operations)),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				});
		};
		var operationMaybe = A2(_elm_lang$core$Dict$get, operationId, model.chain.parsedOperations);
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$h3,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							A2(_elm_lang$core$Basics_ops['++'], 'Operation ', operationId)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: function () {
						var _p23 = operationMaybe;
						if (_p23.ctor === 'Just') {
							return viewOperationFields(_p23._0);
						} else {
							return _elm_lang$html$Html$text('(loading operation data...)');
						}
					}(),
					_1: {ctor: '[]'}
				}
			});
	});
var _user$project$View$viewContract = F2(
	function (contractId, contracts) {
		var viewProg = function (scriptMaybe) {
			return A2(
				_elm_lang$core$Maybe$withDefault,
				_elm_lang$html$Html$text(''),
				A2(
					_elm_lang$core$Maybe$map,
					function (script) {
						return A2(
							_user$project$View$viewProperty,
							'program',
							_user$project$View$viewProgram(
								_user$project$View$simplifyProgram1(script.code.code)));
					},
					scriptMaybe));
		};
		var viewScript = function (scriptMaybe) {
			return A2(
				_elm_lang$core$Maybe$withDefault,
				_elm_lang$html$Html$text(''),
				A2(
					_elm_lang$core$Maybe$map,
					function (script) {
						return A2(
							_user$project$View$viewProperty,
							'script',
							_elm_lang$html$Html$text(
								_elm_lang$core$Basics$toString(script)));
					},
					scriptMaybe));
		};
		var viewDelegate = function (delegate) {
			return A2(
				_elm_lang$html$Html$span,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('hash'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(
								A2(_elm_lang$core$Maybe$withDefault, '', delegate.value)),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							A2(
								_elm_lang$core$Basics_ops['++'],
								' (',
								A2(
									_elm_lang$core$Basics_ops['++'],
									delegate.setable ? 'setable' : 'not setable',
									')'))),
						_1: {ctor: '[]'}
					}
				});
		};
		var contractData = A2(
			_elm_lang$core$Maybe$withDefault,
			_krisajenkins$remotedata$RemoteData$NotAsked,
			A2(_elm_lang$core$Dict$get, contractId, contracts));
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$h3,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('Contract '),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('hash'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(contractId),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}),
				_1: {
					ctor: '::',
					_0: function () {
						var _p24 = contractData;
						switch (_p24.ctor) {
							case 'Success':
								var _p25 = _p24._0;
								return A2(
									_elm_lang$html$Html$div,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: A2(
											_user$project$View$viewProperty,
											'manager',
											A2(
												_elm_lang$html$Html$span,
												{
													ctor: '::',
													_0: _elm_lang$html$Html_Attributes$class('hash'),
													_1: {ctor: '[]'}
												},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text(_p25.manager),
													_1: {ctor: '[]'}
												})),
										_1: {
											ctor: '::',
											_0: A2(
												_user$project$View$viewProperty,
												'balance (ꜩ)',
												_elm_lang$html$Html$text(
													_user$project$View$formatCentiles(_p25.balance))),
											_1: {
												ctor: '::',
												_0: A2(
													_user$project$View$viewProperty,
													'spendable',
													_elm_lang$html$Html$text(
														_elm_lang$core$Basics$toString(_p25.spendable))),
												_1: {
													ctor: '::',
													_0: A2(
														_user$project$View$viewProperty,
														'counter',
														_elm_lang$html$Html$text(
															_elm_lang$core$Basics$toString(_p25.counter))),
													_1: {
														ctor: '::',
														_0: A2(
															_user$project$View$viewProperty,
															'delegate',
															viewDelegate(_p25.delegate)),
														_1: {
															ctor: '::',
															_0: viewProg(_p25.script),
															_1: {
																ctor: '::',
																_0: A2(
																	_elm_lang$html$Html$h5,
																	{ctor: '[]'},
																	{
																		ctor: '::',
																		_0: _elm_lang$html$Html$text('Raw response'),
																		_1: {ctor: '[]'}
																	}),
																_1: {
																	ctor: '::',
																	_0: A2(
																		_elm_lang$html$Html$pre,
																		{ctor: '[]'},
																		{
																			ctor: '::',
																			_0: _elm_lang$html$Html$text(_p25.rawBody),
																			_1: {ctor: '[]'}
																		}),
																	_1: {ctor: '[]'}
																}
															}
														}
													}
												}
											}
										}
									});
							case 'Loading':
								return _elm_lang$html$Html$text('loading ...');
							case 'Failure':
								return _user$project$View$viewFailure(_p24._0);
							default:
								return _elm_lang$html$Html$text('not asked');
						}
					}(),
					_1: {ctor: '[]'}
				}
			});
	});
var _user$project$View$blockFullLink = function (hash) {
	return A2(
		_elm_lang$html$Html$span,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('hash'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$a,
				{
					ctor: '::',
					_0: _user$project$Route$href(
						_user$project$Route$Block(hash)),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(hash),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _user$project$View$viewBlock = F2(
	function (model, block) {
		var viewPropertyList = F2(
			function (label, values) {
				return A2(
					_user$project$View$viewProperty,
					label,
					_elm_lang$html$Html$text(
						_elm_lang$core$String$concat(
							A2(_elm_lang$core$List$intersperse, ', ', values))));
			});
		var viewPropertyString = F2(
			function (label, value) {
				return A2(
					_user$project$View$viewProperty,
					label,
					_elm_lang$html$Html$text(value));
			});
		var viewOperations = function (block) {
			var _p26 = block.operations;
			if (_p26.ctor === 'Just') {
				return A2(
					viewPropertyList,
					'operations',
					A2(
						_elm_lang$core$List$map,
						_user$project$View$shortHash,
						_elm_lang$core$List$concat(_p26._0)));
			} else {
				return A2(viewPropertyString, 'operations', '[unknown]');
			}
		};
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('block'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$h3,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('Block '),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('hash'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(
										_user$project$View$shortHash(block.hash)),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('property-list'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(viewPropertyString, 'hash', block.hash),
							_1: {
								ctor: '::',
								_0: A2(
									_user$project$View$viewProperty,
									'predecessor',
									_user$project$View$blockFullLink(block.predecessor)),
								_1: {
									ctor: '::',
									_0: A2(
										viewPropertyString,
										'timestamp',
										_user$project$View$formatDate(block.timestamp)),
									_1: {
										ctor: '::',
										_0: A2(
											viewPropertyString,
											'level',
											_elm_lang$core$Basics$toString(block.level)),
										_1: {
											ctor: '::',
											_0: A2(
												viewPropertyList,
												'fitness',
												A2(_elm_lang$core$List$map, _elm_lang$core$Basics$toString, block.fitness)),
											_1: {
												ctor: '::',
												_0: A2(viewPropertyString, 'net_id', block.net_id),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$h4,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('Operations'),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(_user$project$View$viewParsedOperations, model, block.hash),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _user$project$View$blockOperationCount = F2(
	function (model, block) {
		var blockOperationsMaybe = A2(_elm_lang$core$Dict$get, block.hash, model.chain.blockOperations);
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (_p27) {
					return _elm_lang$core$Basics$toString(
						_elm_lang$core$List$length(_p27));
				},
				blockOperationsMaybe));
	});
var _user$project$View$viewBlock2 = F4(
	function (model, blockhashMaybe, n, block) {
		return A2(
			_elm_lang$html$Html$tr,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('block'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$td,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							_elm_lang$core$Basics$toString(block.level)),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$td,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('hash'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$title(block.hash),
								_1: {ctor: '[]'}
							}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$a,
								{
									ctor: '::',
									_0: _user$project$Route$href(
										_user$project$Route$Block(block.hash)),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(
										_user$project$View$shortHash(block.hash)),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$td,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('timestamp'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(
									_user$project$View$formatDate(block.timestamp)),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$td,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('age'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(
										A2(_alpacaaa$elm_date_distance$Date_Distance$inWords, model.now, block.timestamp)),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$td,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('operation-count'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											A2(_user$project$View$blockOperationCount, model, block)),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			});
	});
var _user$project$View$viewBranch = F4(
	function (howMany, model, blockhashMaybe, branch) {
		var branchToShow = A2(_elm_lang$core$List$take, howMany, branch);
		var tableHeader = A2(
			_elm_lang$html$Html$tr,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$th,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('level'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$th,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('hash'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$th,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('timestamp'),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$th,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('timestamp'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('age'),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$th,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('operations'),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			});
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('branch'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$table,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('blockchain'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$thead,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: tableHeader,
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$tbody,
										{ctor: '[]'},
										A2(
											_elm_lang$core$List$indexedMap,
											A2(_user$project$View$viewBlock2, model, blockhashMaybe),
											branchToShow)),
									_1: {ctor: '[]'}
								}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	});
var _user$project$View$viewChainAt = F2(
	function (model, hash) {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$h3,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							A2(
								_elm_lang$core$Basics_ops['++'],
								'Chain at ',
								_user$project$View$shortHash(hash))),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A4(
						_user$project$View$viewBranch,
						24,
						model,
						_elm_lang$core$Maybe$Just(hash),
						A2(_user$project$Data_Chain$getBranchList, model.chain, hash)),
					_1: {ctor: '[]'}
				}
			});
	});
var _user$project$View$canonFitness = function (fitness) {
	return A2(
		_elm_community$list_extra$List_Extra$dropWhile,
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(0),
		fitness);
};
var _user$project$View$viewHeader = function (nodeUrl) {
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h1,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Tezos Explorer'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(
							A2(_elm_lang$core$Basics_ops['++'], 'Connecting to Tezos RPC server ', nodeUrl)),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$viewSchemas = function (schemas) {
	var viewSchema = function (name) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			_elm_lang$html$Html$text('failed to get schema from dict'),
			A2(
				_elm_lang$core$Maybe$map,
				function (data) {
					return A2(
						_elm_lang$html$Html$map,
						_user$project$Update$SchemaMsg(name),
						A2(_user$project$Data_Schema$viewSchemaDataTop, name, data));
				},
				A2(_elm_lang$core$Dict$get, name, schemas)));
	};
	var names = _elm_lang$core$Dict$keys(schemas);
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		A2(_elm_lang$core$List$map, viewSchema, names));
};
var _user$project$View$viewHome = function (model) {
	var headMaybe = _elm_lang$core$List$head(model.chain.heads);
	var _p28 = headMaybe;
	if (_p28.ctor === 'Just') {
		var _p29 = _p28._0;
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$h2,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('Newest blocks'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A4(
						_user$project$View$viewBranch,
						24,
						model,
						_elm_lang$core$Maybe$Just(_p29),
						A2(_user$project$Data_Chain$getBranchList, model.chain, _p29)),
					_1: {ctor: '[]'}
				}
			});
	} else {
		return _elm_lang$html$Html$text('no head found yet ...');
	}
};
var _user$project$View$BlockNotFound = function (a) {
	return {ctor: 'BlockNotFound', _0: a};
};
var _user$project$View$BlockFound = function (a) {
	return {ctor: 'BlockFound', _0: a};
};
var _user$project$View$findBlockStatus = F2(
	function (blocks, blockhash) {
		var _p30 = A2(_elm_lang$core$Dict$get, blockhash, blocks);
		if (_p30.ctor === 'Just') {
			return _user$project$View$BlockFound(_p30._0);
		} else {
			return _user$project$View$BlockNotFound(blockhash);
		}
	});
var _user$project$View$viewHeads = function (model) {
	var viewBlockSummary = F2(
		function (i, block) {
			return A2(
				_elm_lang$html$Html$tr,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$td,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('index'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(
								_elm_lang$core$Basics$toString(i)),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$td,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('hash'),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$title(block.hash),
									_1: {ctor: '[]'}
								}
							},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$a,
									{
										ctor: '::',
										_0: _user$project$Route$href(
											_user$project$Route$ChainAt(block.hash)),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											_user$project$View$shortHash(block.hash)),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$td,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('timestamp'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(
										_user$project$View$formatDate(block.timestamp)),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$td,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('age'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											A2(_alpacaaa$elm_date_distance$Date_Distance$inWords, model.now, block.timestamp)),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$td,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('fitness'),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text(
												_elm_lang$core$Basics$toString(
													_user$project$View$canonFitness(block.fitness))),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$td,
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$class('level'),
												_1: {ctor: '[]'}
											},
											{
												ctor: '::',
												_0: _elm_lang$html$Html$text(
													_elm_lang$core$Basics$toString(block.level)),
												_1: {ctor: '[]'}
											}),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				});
		});
	var viewHead = F2(
		function (i, blockStatus) {
			var _p31 = blockStatus;
			if (_p31.ctor === 'BlockFound') {
				return A2(viewBlockSummary, i, _p31._0);
			} else {
				return _elm_lang$html$Html$text(
					A2(
						_elm_lang$core$Basics_ops['++'],
						'block ',
						A2(_elm_lang$core$Basics_ops['++'], _p31._0, ' not found')));
			}
		});
	var header = A2(
		_elm_lang$html$Html$tr,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$th,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('index'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('index'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$th,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('hash'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('hash'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$th,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('timestamp'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('timestamp'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$th,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('age'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('age'),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$th,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('fitness'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('fitness'),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$th,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('level'),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('level'),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}
		});
	var heads = A2(
		_elm_lang$core$List$map,
		_user$project$View$findBlockStatus(model.chain.blocks),
		model.chain.heads);
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$h2,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Blockchain heads'),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('heads'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$table,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('heads'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$thead,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: header,
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$tbody,
										{ctor: '[]'},
										A2(_elm_lang$core$List$indexedMap, viewHead, heads)),
									_1: {ctor: '[]'}
								}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$View$view = function (model) {
	var content = function () {
		var _p32 = model.pageState;
		switch (_p32._0.ctor) {
			case 'Home':
				return _user$project$View$viewHome(model);
			case 'Blank':
				return _elm_lang$html$Html$text('');
			case 'Operations':
				return _user$project$View$viewAllOperations(model);
			case 'Operation':
				return A2(_user$project$View$viewOperation, model, _p32._0._0);
			case 'Schema':
				return _user$project$View$viewSchemas(model.schemaData);
			case 'Debug':
				return _user$project$View$viewDebug(model);
			case 'Heads':
				return _user$project$View$viewHeads(model);
			case 'Block':
				var _p33 = A2(_elm_lang$core$Dict$get, _p32._0._0, model.chain.blocks);
				if (_p33.ctor === 'Just') {
					return A2(_user$project$View$viewBlock, model, _p33._0);
				} else {
					return _elm_lang$html$Html$text('loading block ...');
				}
			case 'ChainAt':
				return A2(_user$project$View$viewChainAt, model, _p32._0._0);
			case 'Contracts':
				return _user$project$View$viewContracts(model);
			case 'Keys':
				return _user$project$View$viewKeys(model.chain.keys);
			case 'Peers':
				return _user$project$View$viewPeers(model.chain.peers);
			case 'Contract':
				return A2(_user$project$View$viewContract, _p32._0._0, model.chain.contracts);
			case 'Errors':
				return A2(_user$project$View$viewError, model.nodeUrl, model.errors);
			case 'NotFound':
				return _elm_lang$html$Html$text('page not found');
			default:
				return _user$project$View$viewAbout(model);
		}
	}();
	var context = {
		pageState: model.pageState,
		now: model.now,
		errorCount: _elm_lang$core$List$length(model.errors)
	};
	return A2(_user$project$View_Page$frame, context, content);
};

var _user$project$Main$init = F2(
	function (flags, location) {
		var initModel = {
			schemaData: _elm_lang$core$Dict$empty,
			errors: {ctor: '[]'},
			nodeUrl: flags.nodeUrl,
			chain: _user$project$Data_Chain$init,
			now: _elm_lang$core$Date$fromTime(flags.now),
			pageState: _user$project$Model$Loaded(_user$project$Page$Blank)
		};
		var _p0 = A2(
			_user$project$Update$setRoute,
			_user$project$Route$fromLocation(location),
			initModel);
		var routedModel = _p0._0;
		var routeCmd = _p0._1;
		return {
			ctor: '_Tuple2',
			_0: routedModel,
			_1: _elm_lang$core$Platform_Cmd$batch(
				{
					ctor: '::',
					_0: A2(
						_elm_lang$http$Http$send,
						function (_p1) {
							return _user$project$Update$RpcResponse(
								A2(_elm_lang$core$Result$map, _user$project$Request$Heads, _p1));
						},
						_user$project$Request_Block$getHeads(routedModel.nodeUrl)),
					_1: {
						ctor: '::',
						_0: routeCmd,
						_1: {ctor: '[]'}
					}
				})
		};
	});
var _user$project$Main$monitor = _elm_lang$core$Native_Platform.incomingPort('monitor', _elm_lang$core$Json_Decode$value);
var _user$project$Main$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$batch(
		{
			ctor: '::',
			_0: A2(_elm_lang$core$Time$every, 10 * _elm_lang$core$Time$minute, _user$project$Update$Tick),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Time$every, 30 * _elm_lang$core$Time$second, _user$project$Update$Now),
				_1: {
					ctor: '::',
					_0: _user$project$Main$monitor(_user$project$Update$Monitor),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Main$main = A2(
	_elm_lang$navigation$Navigation$programWithFlags,
	function (_p2) {
		return _user$project$Update$SetRoute(
			_user$project$Route$fromLocation(_p2));
	},
	{init: _user$project$Main$init, update: _user$project$Update$update, view: _user$project$View$view, subscriptions: _user$project$Main$subscriptions})(
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (nodeUrl) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (now) {
					return _elm_lang$core$Json_Decode$succeed(
						{nodeUrl: nodeUrl, now: now});
				},
				A2(_elm_lang$core$Json_Decode$field, 'now', _elm_lang$core$Json_Decode$float));
		},
		A2(_elm_lang$core$Json_Decode$field, 'nodeUrl', _elm_lang$core$Json_Decode$string)));
var _user$project$Main$Flags = F2(
	function (a, b) {
		return {nodeUrl: a, now: b};
	});

var Elm = {};
Elm['Main'] = Elm['Main'] || {};
if (typeof _user$project$Main$main !== 'undefined') {
    _user$project$Main$main(Elm['Main'], 'Main', undefined);
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

