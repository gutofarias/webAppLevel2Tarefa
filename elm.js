(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
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


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
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

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

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


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
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
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
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

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$ModelSystem$Level2 = {$: 'Level2'};
var $author$project$Main$SolvingEdo = function (a) {
	return {$: 'SolvingEdo', a: a};
};
var $author$project$Controller$PID = {$: 'PID'};
var $author$project$Controller$PidModel = function (a) {
	return {$: 'PidModel', a: a};
};
var $author$project$Controller$PideqModel = function (a) {
	return {$: 'PideqModel', a: a};
};
var $author$project$Controller$PID$init = {kd: 0.0, kdStr: '0', ki: 0.0, kiStr: '0', kp: 0.0, kpStr: '0'};
var $author$project$Controller$PIDeq$init = {kd: 0.0, kdStr: '0', ki: 0.0, kiStr: '0', kp: 0.0, kpStr: '0', ueq: 0.0, ueqStr: '0'};
var $author$project$Controller$init = function (controlType) {
	if (controlType.$ === 'PID') {
		return $author$project$Controller$PidModel($author$project$Controller$PID$init);
	} else {
		return $author$project$Controller$PideqModel($author$project$Controller$PIDeq$init);
	}
};
var $author$project$ModelSystem$Level$control = $author$project$Controller$init($author$project$Controller$PID);
var $author$project$Controller$PIDeq = {$: 'PIDeq'};
var $author$project$ModelSystem$Level2$control = $author$project$Controller$init($author$project$Controller$PIDeq);
var $author$project$ModelSystem$control = function (modelType) {
	if (modelType.$ === 'Level') {
		return $author$project$ModelSystem$Level$control;
	} else {
		return $author$project$ModelSystem$Level2$control;
	}
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$EdoSolver$edoIStatesFromEdoParam = function (edoParam) {
	var tiniStr = $elm$core$String$fromFloat(
		function ($) {
			return $.tempo;
		}(edoParam));
	var tfimStr = $elm$core$String$fromFloat(
		function ($) {
			return $.tfim;
		}(edoParam));
	return {tfimStr: tfimStr, tiniStr: tiniStr};
};
var $author$project$ModelSystem$Level2Model = function (a) {
	return {$: 'Level2Model', a: a};
};
var $author$project$ModelSystem$LevelModel = function (a) {
	return {$: 'LevelModel', a: a};
};
var $author$project$ModelSystem$Level$init = {ag: 1.0, agStr: '1', ap: 0.1, apStr: '0.1', h0: 10.0, h0Str: '10'};
var $author$project$ModelSystem$Level2$init = {ag1: 1.0, ag1Str: '1', ag2: 1.0, ag2Str: '1', ap1: 0.1, ap1Str: '0.1', ap2: 0.1, ap2Str: '0.1', h01: 10.0, h01Str: '10', h02: 10.0, h02Str: '10'};
var $author$project$ModelSystem$init = function (modelType) {
	if (modelType.$ === 'Level') {
		return $author$project$ModelSystem$LevelModel($author$project$ModelSystem$Level$init);
	} else {
		return $author$project$ModelSystem$Level2Model($author$project$ModelSystem$Level2$init);
	}
};
var $author$project$MyCharts$Chart$Curve$init = function (maybeLastCurve) {
	if (maybeLastCurve.$ === 'Nothing') {
		return {
			axesString: _Utils_Tuple2('t', 'x1'),
			curveID: 1
		};
	} else {
		var lastCurve = maybeLastCurve.a;
		var lastCurveID = function ($) {
			return $.curveID;
		}(lastCurve);
		var curveID = lastCurveID + 1;
		return {
			axesString: _Utils_Tuple2('t', 'x1'),
			curveID: curveID
		};
	}
};
var $author$project$MyCharts$Chart$Zoom$NoZoom = {$: 'NoZoom'};
var $author$project$MyCharts$Chart$Zoom$init = $author$project$MyCharts$Chart$Zoom$NoZoom;
var $author$project$UI$elementHeight = 400;
var $author$project$UI$elementWidth = 400;
var $gampleman$elm_visualization$Zoom$NoTouches = {$: 'NoTouches'};
var $gampleman$elm_visualization$Zoom$Zoom = function (a) {
	return {$: 'Zoom', a: a};
};
var $gampleman$elm_visualization$Zoom$Transform$Transform = F3(
	function (k, x, y) {
		return {k: k, x: x, y: y};
	});
var $gampleman$elm_visualization$Zoom$Transform$identity = A3($gampleman$elm_visualization$Zoom$Transform$Transform, 1.0, 0.0, 0.0);
var $gampleman$elm_visualization$Zoom$infinity = 1 / 0;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $gampleman$elm_visualization$Zoom$init = function (_v0) {
	var width = _v0.width;
	var height = _v0.height;
	return $gampleman$elm_visualization$Zoom$Zoom(
		{
			drag: $elm$core$Maybe$Nothing,
			extent: _Utils_Tuple2(
				_Utils_Tuple2(0, 0),
				_Utils_Tuple2(width, height)),
			gestureLastScale: 0,
			scaleExtent: _Utils_Tuple2(0, $gampleman$elm_visualization$Zoom$infinity),
			touchStarted: false,
			touches: $gampleman$elm_visualization$Zoom$NoTouches,
			transform: $gampleman$elm_visualization$Zoom$Transform$identity,
			transition: $elm$core$Maybe$Nothing,
			translateExtent: _Utils_Tuple2(
				_Utils_Tuple2(-$gampleman$elm_visualization$Zoom$infinity, -$gampleman$elm_visualization$Zoom$infinity),
				_Utils_Tuple2($gampleman$elm_visualization$Zoom$infinity, $gampleman$elm_visualization$Zoom$infinity))
		});
};
var $gampleman$elm_visualization$Zoom$scaleExtent = F3(
	function (mn, mx, _v0) {
		var zoom = _v0.a;
		return $gampleman$elm_visualization$Zoom$Zoom(
			_Utils_update(
				zoom,
				{
					scaleExtent: _Utils_Tuple2(mn, mx)
				}));
	});
var $author$project$MyCharts$Chart$myInitZoom = A3(
	$gampleman$elm_visualization$Zoom$scaleExtent,
	0.5,
	1000.0,
	$gampleman$elm_visualization$Zoom$init(
		{height: $author$project$UI$elementHeight, width: $author$project$UI$elementWidth}));
var $author$project$MyCharts$Chart$init = function (maybeLastChart) {
	if (maybeLastChart.$ === 'Nothing') {
		return {
			chartID: 1,
			curves: _List_fromArray(
				[
					$author$project$MyCharts$Chart$Curve$init($elm$core$Maybe$Nothing)
				]),
			editingCurves: false,
			zoom: $author$project$MyCharts$Chart$myInitZoom,
			zoomModel: $author$project$MyCharts$Chart$Zoom$init
		};
	} else {
		var lastChart = maybeLastChart.a;
		var lastChartID = function ($) {
			return $.chartID;
		}(lastChart);
		var chartID = lastChartID + 1;
		return {
			chartID: chartID,
			curves: _List_fromArray(
				[
					$author$project$MyCharts$Chart$Curve$init($elm$core$Maybe$Nothing)
				]),
			editingCurves: false,
			zoom: $author$project$MyCharts$Chart$myInitZoom,
			zoomModel: $author$project$MyCharts$Chart$Zoom$init
		};
	}
};
var $author$project$MyCharts$init = _List_fromArray(
	[
		$author$project$MyCharts$Chart$init($elm$core$Maybe$Nothing)
	]);
var $author$project$EdoSolver$zipWith = F3(
	function (func, l1, l2) {
		if (!l1.b) {
			return _List_Nil;
		} else {
			if (!l1.b.b) {
				var x = l1.a;
				if (!l2.b) {
					return _List_Nil;
				} else {
					if (!l2.b.b) {
						var y = l2.a;
						return A2(
							$elm$core$List$cons,
							A2(func, x, y),
							_List_Nil);
					} else {
						var y = l2.a;
						var ys = l2.b;
						return A2(
							$elm$core$List$cons,
							A2(func, x, y),
							_List_Nil);
					}
				}
			} else {
				var x = l1.a;
				var xs = l1.b;
				if (!l2.b) {
					return _List_Nil;
				} else {
					if (!l2.b.b) {
						var y = l2.a;
						return A2(
							$elm$core$List$cons,
							A2(func, x, y),
							_List_Nil);
					} else {
						var y = l2.a;
						var ys = l2.b;
						return A2(
							$elm$core$List$cons,
							A2(func, x, y),
							A3($author$project$EdoSolver$zipWith, func, xs, ys));
					}
				}
			}
		}
	});
var $author$project$EdoSolver$rungeKutta = F4(
	function (fsist, passo, tempo, xs) {
		var xps1 = A2(fsist, tempo, xs);
		var tempo4 = tempo + passo;
		var tempo2 = tempo + (0.5 * passo);
		var tempo3 = tempo2;
		var k1 = A2(
			$elm$core$List$map,
			$elm$core$Basics$mul(passo),
			xps1);
		var xs2 = A3(
			$author$project$EdoSolver$zipWith,
			F2(
				function (a, b) {
					return a + (0.5 * b);
				}),
			xs,
			k1);
		var xps2 = A2(fsist, tempo2, xs2);
		var k2 = A2(
			$elm$core$List$map,
			$elm$core$Basics$mul(passo),
			xps2);
		var xs3 = A3(
			$author$project$EdoSolver$zipWith,
			F2(
				function (a, b) {
					return a + (0.5 * b);
				}),
			xs,
			k2);
		var xps3 = A2(fsist, tempo3, xs3);
		var k3 = A2(
			$elm$core$List$map,
			$elm$core$Basics$mul(passo),
			xps3);
		var xs4 = A3($author$project$EdoSolver$zipWith, $elm$core$Basics$add, k3, xs);
		var xps4 = A2(fsist, tempo4, xs4);
		var k4 = A2(
			$elm$core$List$map,
			$elm$core$Basics$mul(passo),
			xps4);
		return A3(
			$author$project$EdoSolver$zipWith,
			F2(
				function (a, b) {
					return a + (b / 6.0);
				}),
			xs,
			A3(
				$author$project$EdoSolver$zipWith,
				$elm$core$Basics$add,
				k1,
				A3(
					$author$project$EdoSolver$zipWith,
					F2(
						function (a, b) {
							return (2.0 * a) + b;
						}),
					k2,
					A3(
						$author$project$EdoSolver$zipWith,
						F2(
							function (a, b) {
								return (2.0 * a) + b;
							}),
						k3,
						k4))));
	});
var $author$project$ModelSystem$Level$initEdoParam = {controlMemory: _List_Nil, passo: 0.001, relPassoSaida: 100, solver: $author$project$EdoSolver$rungeKutta, tempo: 0.0, tfim: 10.0};
var $author$project$ModelSystem$Level2$initEdoParam = {controlMemory: _List_Nil, passo: 0.1, relPassoSaida: 100, solver: $author$project$EdoSolver$rungeKutta, tempo: 0.0, tfim: 10.0};
var $author$project$ModelSystem$initEdoParam = function (modelType) {
	if (modelType.$ === 'Level') {
		return $author$project$ModelSystem$Level$initEdoParam;
	} else {
		return $author$project$ModelSystem$Level2$initEdoParam;
	}
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Reference$Step1 = {$: 'Step1'};
var $author$project$Reference$Step1Model = function (a) {
	return {$: 'Step1Model', a: a};
};
var $author$project$Reference$Step1$init = {fVal: 0.0, fValStr: '0', iVal: 0.0, iValStr: '0', tStep: 0.0, tStepStr: '0'};
var $author$project$Reference$init = function (refType) {
	return $author$project$Reference$Step1Model($author$project$Reference$Step1$init);
};
var $author$project$ModelSystem$Level$ref = $author$project$Reference$init($author$project$Reference$Step1);
var $author$project$ModelSystem$Level2$ref = $author$project$Reference$init($author$project$Reference$Step1);
var $author$project$ModelSystem$ref = function (modelType) {
	if (modelType.$ === 'Level') {
		return $author$project$ModelSystem$Level$ref;
	} else {
		return $author$project$ModelSystem$Level2$ref;
	}
};
var $author$project$Main$init = function (_v0) {
	var modelSystemType = $author$project$ModelSystem$Level2;
	var edoParam = $author$project$ModelSystem$initEdoParam(modelSystemType);
	var edoIStates = $author$project$EdoSolver$edoIStatesFromEdoParam(edoParam);
	return _Utils_Tuple2(
		$author$project$Main$SolvingEdo(
			{
				chartData: _List_Nil,
				chartsModel: $author$project$MyCharts$init,
				controlModel: $author$project$ModelSystem$control(modelSystemType),
				edoIStates: edoIStates,
				edoParam: edoParam,
				modelSystemModel: $author$project$ModelSystem$init(modelSystemType),
				modelSystemType: modelSystemType,
				refModel: $author$project$ModelSystem$ref(modelSystemType)
			}),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Main$MyChartsMsg = function (a) {
	return {$: 'MyChartsMsg', a: a};
};
var $author$project$Main$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.request;
		var oldTime = _v0.oldTime;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 'Nothing') {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.subs;
		var oldTime = _v0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrameDelta = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Delta(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrameDelta = $elm$browser$Browser$AnimationManager$onAnimationFrameDelta;
var $author$project$MyCharts$ChartMsg = F2(
	function (a, b) {
		return {$: 'ChartMsg', a: a, b: b};
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$MyCharts$Chart$EVZoomMsg = function (a) {
	return {$: 'EVZoomMsg', a: a};
};
var $gampleman$elm_visualization$Zoom$MouseMove = function (a) {
	return {$: 'MouseMove', a: a};
};
var $gampleman$elm_visualization$Zoom$MouseUp = {$: 'MouseUp'};
var $gampleman$elm_visualization$Zoom$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $gampleman$elm_visualization$Zoom$TouchStartedExpired = {$: 'TouchStartedExpired'};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onMouseMove = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mousemove');
var $elm$browser$Browser$Events$onMouseUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mouseup');
var $gampleman$elm_visualization$Zoom$touchDelay = 500;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $gampleman$elm_visualization$Zoom$subscriptions = F2(
	function (_v0, tagger) {
		var zoom = _v0.a;
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					function () {
					var _v1 = zoom.drag;
					if (_v1.$ === 'Just') {
						return $elm$core$Platform$Sub$batch(
							_List_fromArray(
								[
									$elm$browser$Browser$Events$onMouseMove(
									A3(
										$elm$json$Json$Decode$map2,
										F2(
											function (x, y) {
												return tagger(
													$gampleman$elm_visualization$Zoom$MouseMove(
														_Utils_Tuple2(x, y)));
											}),
										A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
										A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float))),
									$elm$browser$Browser$Events$onMouseUp(
									$elm$json$Json$Decode$succeed(
										tagger($gampleman$elm_visualization$Zoom$MouseUp)))
								]));
					} else {
						return $elm$core$Platform$Sub$none;
					}
				}(),
					zoom.touchStarted ? A2(
					$elm$time$Time$every,
					$gampleman$elm_visualization$Zoom$touchDelay,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Basics$always($gampleman$elm_visualization$Zoom$TouchStartedExpired),
						tagger)) : $elm$core$Platform$Sub$none,
					A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Platform$Sub$none,
					A2(
						$elm$core$Maybe$map,
						$elm$core$Basics$always(
							$elm$browser$Browser$Events$onAnimationFrameDelta(
								A2($elm$core$Basics$composeR, $gampleman$elm_visualization$Zoom$Tick, tagger))),
						zoom.transition))
				]));
	});
var $author$project$MyCharts$Chart$subscriptions = F2(
	function (model, msgToMainMsg) {
		return A2(
			$gampleman$elm_visualization$Zoom$subscriptions,
			model.zoom,
			A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$MyCharts$Chart$EVZoomMsg));
	});
var $author$project$MyCharts$subscriptions = F2(
	function (model, msgToMainMsg) {
		if (!model.b) {
			return $elm$core$Platform$Sub$none;
		} else {
			var chs = model;
			return $elm$core$Platform$Sub$batch(
				A2(
					$elm$core$List$map,
					function (ch) {
						return A2(
							$author$project$MyCharts$Chart$subscriptions,
							ch,
							A2(
								$elm$core$Basics$composeL,
								msgToMainMsg,
								$author$project$MyCharts$ChartMsg(ch.chartID)));
					},
					chs));
		}
	});
var $author$project$Main$subscriptions = function (bigModel) {
	if (bigModel.$ === 'SolvingEdo') {
		var model = bigModel.a;
		return A2($author$project$MyCharts$subscriptions, model.chartsModel, $author$project$Main$MyChartsMsg);
	} else {
		return $elm$browser$Browser$Events$onAnimationFrameDelta($author$project$Main$Tick);
	}
};
var $author$project$Main$Animation = F3(
	function (a, b, c) {
		return {$: 'Animation', a: a, b: b, c: c};
	});
var $author$project$Main$UpdateEdoParam = {$: 'UpdateEdoParam'};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $author$project$EdoSolver$eps = 1e-8;
var $author$project$EdoSolver$checkEndTimeEps = F2(
	function (tempo, tfim) {
		return (_Utils_cmp(
			$elm$core$Basics$abs(tempo - tfim),
			$author$project$EdoSolver$eps) < 1) ? true : false;
	});
var $author$project$EdoSolver$eulerSolver = F4(
	function (fsist, passo, tempo, xs) {
		var xps = A2(fsist, tempo, xs);
		var xps2 = A2(
			$elm$core$List$map,
			$elm$core$Basics$mul(passo),
			xps);
		return A3($author$project$EdoSolver$zipWith, $elm$core$Basics$add, xs, xps2);
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Controller$PID$pid = F8(
	function (kp, ki, kd, mem, errors, passo, tempo, xs) {
		var filterParam = 0.05;
		var fP = 1.0 / filterParam;
		var error = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$head(errors));
		var errorfsist = F2(
			function (t, xs2) {
				if (xs2.b && (!xs2.b.b)) {
					var x = xs2.a;
					return A2($elm$core$List$cons, ((-fP) * x) - ((fP * fP) * error), _List_Nil);
				} else {
					return A2($elm$core$List$cons, (-fP) * error, _List_Nil);
				}
			});
		var prop = kp * error;
		var _v0 = function () {
			if ((mem.b && mem.b.b) && (!mem.b.b.b)) {
				var err_mem = mem.a;
				var _v2 = mem.b;
				var int_mem = _v2.a;
				return _Utils_Tuple2(err_mem, int_mem);
			} else {
				return _Utils_Tuple2((-fP) * error, 0.0);
			}
		}();
		var error_state = _v0.a;
		var integral_mem = _v0.b;
		var derror_aux = A2(
			$elm$core$Maybe$withDefault,
			(-fP) * error,
			$elm$core$List$head(
				A4(
					$author$project$EdoSolver$eulerSolver,
					errorfsist,
					passo,
					tempo,
					_List_fromArray(
						[error_state]))));
		var derror = derror_aux + (fP * error);
		var dif = kd * derror;
		var integral_error = integral_mem + (error * passo);
		var integral = ki * integral_error;
		return _Utils_Tuple2(
			_List_fromArray(
				[(prop + integral) + dif, prop, integral, dif]),
			_List_fromArray(
				[derror_aux, integral_error]));
	});
var $author$project$Controller$PID$controllerFromModel = function (model) {
	var kp = function ($) {
		return $.kp;
	}(model);
	var ki = function ($) {
		return $.ki;
	}(model);
	var kd = function ($) {
		return $.kd;
	}(model);
	return A3($author$project$Controller$PID$pid, kp, ki, kd);
};
var $author$project$Controller$PIDeq$pideq = F9(
	function (kp, ki, kd, ueq, mem, errors, passo, tempo, xs) {
		var filterParam = 0.05;
		var fP = 1.0 / filterParam;
		var error = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$head(errors));
		var errorfsist = F2(
			function (t, xs2) {
				if (xs2.b && (!xs2.b.b)) {
					var x = xs2.a;
					return A2($elm$core$List$cons, ((-fP) * x) - ((fP * fP) * error), _List_Nil);
				} else {
					return A2($elm$core$List$cons, (-fP) * error, _List_Nil);
				}
			});
		var prop = kp * error;
		var _v0 = function () {
			if ((mem.b && mem.b.b) && (!mem.b.b.b)) {
				var err_mem = mem.a;
				var _v2 = mem.b;
				var int_mem = _v2.a;
				return _Utils_Tuple2(err_mem, int_mem);
			} else {
				return _Utils_Tuple2((-fP) * error, 0.0);
			}
		}();
		var error_state = _v0.a;
		var integral_mem = _v0.b;
		var derror_aux = A2(
			$elm$core$Maybe$withDefault,
			(-fP) * error,
			$elm$core$List$head(
				A4(
					$author$project$EdoSolver$eulerSolver,
					errorfsist,
					passo,
					tempo,
					_List_fromArray(
						[error_state]))));
		var derror = derror_aux + (fP * error);
		var dif = kd * derror;
		var integral_error = integral_mem + (error * passo);
		var integral = ki * integral_error;
		return _Utils_Tuple2(
			_List_fromArray(
				[((prop + integral) + dif) + ueq, prop, integral, dif]),
			_List_fromArray(
				[derror_aux, integral_error]));
	});
var $author$project$Controller$PIDeq$controllerFromModel = function (model) {
	var ueq = function ($) {
		return $.ueq;
	}(model);
	var kp = function ($) {
		return $.kp;
	}(model);
	var ki = function ($) {
		return $.ki;
	}(model);
	var kd = function ($) {
		return $.kd;
	}(model);
	return A4($author$project$Controller$PIDeq$pideq, kp, ki, kd, ueq);
};
var $author$project$Controller$controllerFromModel = function (model) {
	if (model.$ === 'PidModel') {
		var pidModel = model.a;
		return $author$project$Controller$PID$controllerFromModel(pidModel);
	} else {
		var pideqModel = model.a;
		return $author$project$Controller$PIDeq$controllerFromModel(pideqModel);
	}
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $author$project$Main$modelFromBigModel = function (bigModel) {
	if (bigModel.$ === 'SolvingEdo') {
		var model = bigModel.a;
		return model;
	} else {
		var model = bigModel.a;
		return model;
	}
};
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Reference$Step1$step1 = F5(
	function (iVal, tStep, fVal, tempo, output) {
		return (_Utils_cmp(tStep, tempo) > -1) ? _List_fromArray(
			[iVal]) : _List_fromArray(
			[fVal]);
	});
var $author$project$Reference$Step1$refFunctionFromModel = function (model) {
	var tStep = function ($) {
		return $.tStep;
	}(model);
	var iVal = function ($) {
		return $.iVal;
	}(model);
	var fVal = function ($) {
		return $.fVal;
	}(model);
	return A3($author$project$Reference$Step1$step1, iVal, tStep, fVal);
};
var $author$project$Reference$refFunctionFromModel = function (model) {
	var step1Model = model.a;
	return $author$project$Reference$Step1$refFunctionFromModel(step1Model);
};
var $author$project$EdoSolver$Controlled = function (a) {
	return {$: 'Controlled', a: a};
};
var $author$project$EdoSolver$calcXsAndMaybeUR = F3(
	function (param, edoSist, xs) {
		if (edoSist.$ === 'Uncontrolled') {
			var sistFunc = edoSist.a;
			return xs;
		} else {
			var functions = edoSist.a;
			var tempo = function ($) {
				return $.tempo;
			}(param);
			var refFunc = function ($) {
				return $.refFunc;
			}(functions);
			var passo = function ($) {
				return $.passo;
			}(param);
			var outputFunc = function ($) {
				return $.outputFunc;
			}(functions);
			var output = A2(outputFunc, tempo, xs);
			var ref = A2(refFunc, tempo, output);
			var error = A3($author$project$EdoSolver$zipWith, $elm$core$Basics$sub, ref, output);
			var controller = function ($) {
				return $.controller;
			}(functions);
			var controlMem = function ($) {
				return $.controlMemory;
			}(param);
			var _v1 = A5(controller, controlMem, error, passo, tempo, xs);
			var controlEffort = _v1.a;
			var newControlMem = _v1.b;
			return A3(
				$elm$core$List$foldr,
				$elm$core$Basics$append,
				_List_Nil,
				_List_fromArray(
					[xs, error, ref, controlEffort, output]));
		}
	});
var $author$project$EdoSolver$calcFsist = F3(
	function (param, edoSist, xs) {
		if (edoSist.$ === 'Uncontrolled') {
			var sistFunc = edoSist.a;
			return _Utils_Tuple2(sistFunc, param);
		} else {
			var functions = edoSist.a;
			var tempo = function ($) {
				return $.tempo;
			}(param);
			var sistFunc = function ($) {
				return $.sistFunc;
			}(functions);
			var refFunc = function ($) {
				return $.refFunc;
			}(functions);
			var passo = function ($) {
				return $.passo;
			}(param);
			var outputFunc = function ($) {
				return $.outputFunc;
			}(functions);
			var output = A2(outputFunc, tempo, xs);
			var ref = A2(refFunc, tempo, output);
			var error = A3($author$project$EdoSolver$zipWith, $elm$core$Basics$sub, ref, output);
			var controller = function ($) {
				return $.controller;
			}(functions);
			var controlMem = function ($) {
				return $.controlMemory;
			}(param);
			var _v1 = A5(controller, controlMem, error, passo, tempo, xs);
			var controlEffort = _v1.a;
			var newControlMem = _v1.b;
			var fsist = sistFunc(controlEffort);
			var newParam = _Utils_update(
				param,
				{controlMemory: newControlMem});
			return _Utils_Tuple2(fsist, newParam);
		}
	});
var $author$project$EdoSolver$edoStep = F3(
	function (param, fsist, xs) {
		var tempo = function ($) {
			return $.tempo;
		}(param);
		var solver = function ($) {
			return $.solver;
		}(param);
		var passo = function ($) {
			return $.passo;
		}(param);
		return A4(solver, fsist, passo, tempo, xs);
	});
var $author$project$EdoSolver$integrator = F4(
	function (param, edoSist, saidaCount, xs) {
		integrator:
		while (true) {
			var tfim = function ($) {
				return $.tfim;
			}(param);
			var tempo = function ($) {
				return $.tempo;
			}(param);
			var relPassoSaida = function ($) {
				return $.relPassoSaida;
			}(param);
			var passo = function ($) {
				return $.passo;
			}(param);
			var controlMem = function ($) {
				return $.controlMemory;
			}(param);
			if (_Utils_cmp(tfim - tempo, passo) < 1) {
				if (_Utils_cmp(
					$elm$core$Basics$abs(tfim - tempo),
					$author$project$EdoSolver$eps) < 1) {
					var paramFinal = _Utils_update(
						param,
						{tempo: tfim});
					return _Utils_Tuple2(
						_Utils_Tuple2(tfim, xs),
						paramFinal);
				} else {
					var newParam = _Utils_update(
						param,
						{passo: tfim - tempo});
					var _v0 = A3($author$project$EdoSolver$calcFsist, newParam, edoSist, xs);
					var fsist = _v0.a;
					var param2 = _v0.b;
					var paramfinal = _Utils_update(
						param2,
						{passo: passo});
					var xsfinal = A3($author$project$EdoSolver$edoStep, param2, fsist, xs);
					return _Utils_Tuple2(
						_Utils_Tuple2(tfim, xsfinal),
						paramfinal);
				}
			} else {
				var tempo1 = tempo + passo;
				var _v1 = A3($author$project$EdoSolver$calcFsist, param, edoSist, xs);
				var fsist = _v1.a;
				var newParam = _v1.b;
				var param1 = _Utils_update(
					newParam,
					{tempo: tempo1});
				var xs1 = A3($author$project$EdoSolver$edoStep, newParam, fsist, xs);
				if (_Utils_eq(saidaCount, relPassoSaida)) {
					return _Utils_Tuple2(
						_Utils_Tuple2(tempo1, xs1),
						param1);
				} else {
					var $temp$param = param1,
						$temp$edoSist = edoSist,
						$temp$saidaCount = saidaCount + 1,
						$temp$xs = xs1;
					param = $temp$param;
					edoSist = $temp$edoSist;
					saidaCount = $temp$saidaCount;
					xs = $temp$xs;
					continue integrator;
				}
			}
		}
	});
var $author$project$EdoSolver$edoSolverAcc = F4(
	function (param, edoSist, xs, _v0) {
		edoSolverAcc:
		while (true) {
			var data = _v0.a;
			var param2 = _v0.b;
			var tfim = function ($) {
				return $.tfim;
			}(param);
			var tempo = function ($) {
				return $.tempo;
			}(param);
			if (_Utils_cmp(tempo, tfim) > -1) {
				return _Utils_Tuple2(data, param2);
			} else {
				var _v1 = A4($author$project$EdoSolver$integrator, param, edoSist, 1, xs);
				var _v2 = _v1.a;
				var tempo1 = _v2.a;
				var xs1 = _v2.b;
				var param1 = _v1.b;
				var param1b = _Utils_update(
					param1,
					{tempo: tempo1});
				var xsAndMaybeUR = A3($author$project$EdoSolver$calcXsAndMaybeUR, param1b, edoSist, xs1);
				var $temp$param = param1b,
					$temp$edoSist = edoSist,
					$temp$xs = xs1,
					$temp$_v0 = _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(tempo1, xsAndMaybeUR),
						data),
					param1b);
				param = $temp$param;
				edoSist = $temp$edoSist;
				xs = $temp$xs;
				_v0 = $temp$_v0;
				continue edoSolverAcc;
			}
		}
	});
var $author$project$EdoSolver$edoSolverReversed = F3(
	function (param, edoSist, xs) {
		var xsAndMaybeUR = A3($author$project$EdoSolver$calcXsAndMaybeUR, param, edoSist, xs);
		var tempo = function ($) {
			return $.tempo;
		}(param);
		var _v0 = A4(
			$author$project$EdoSolver$edoSolverAcc,
			param,
			edoSist,
			xs,
			_Utils_Tuple2(
				A2(
					$elm$core$List$cons,
					_Utils_Tuple2(tempo, xsAndMaybeUR),
					_List_Nil),
				param));
		var reversedData = _v0.a;
		var paramFinal = _v0.b;
		return _Utils_Tuple2(reversedData, paramFinal);
	});
var $author$project$ModelSystem$Level$output = F2(
	function (tempo, xs) {
		if (xs.b) {
			var x = xs.a;
			var ls = xs.b;
			return _List_fromArray(
				[x]);
		} else {
			return xs;
		}
	});
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $author$project$ModelSystem$Level$system = F4(
	function (model, us, t, state) {
		var uAux = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$head(us));
		var u = (uAux < 0.0) ? 0.0 : uAux;
		var g = 9.28;
		var ap = function ($) {
			return $.ap;
		}(model);
		var ag = function ($) {
			return $.ag;
		}(model);
		if (state.b) {
			var h = state.a;
			var ls = state.b;
			var hn = (h >= 0.0) ? h : 0.0;
			return A2(
				$elm$core$List$cons,
				(((-(ap / ag)) * $elm$core$Basics$sqrt(2.0 * g)) * $elm$core$Basics$sqrt(hn)) + (u / ag),
				_List_Nil);
		} else {
			return A2($elm$core$List$cons, 0.0, _List_Nil);
		}
	});
var $author$project$DataConvert$TS1E1R1U4 = function (a) {
	return {$: 'TS1E1R1U4', a: a};
};
var $author$project$DataConvert$toChartDatumTS1E1R1U4 = function (edoDatum) {
	var tempo = edoDatum.a;
	var sist = edoDatum.b;
	if ((((((sist.b && sist.b.b) && sist.b.b.b) && sist.b.b.b.b) && sist.b.b.b.b.b) && sist.b.b.b.b.b.b) && sist.b.b.b.b.b.b.b) {
		var x = sist.a;
		var _v2 = sist.b;
		var e = _v2.a;
		var _v3 = _v2.b;
		var r = _v3.a;
		var _v4 = _v3.b;
		var u1 = _v4.a;
		var _v5 = _v4.b;
		var u2 = _v5.a;
		var _v6 = _v5.b;
		var u3 = _v6.a;
		var _v7 = _v6.b;
		var u4 = _v7.a;
		var xs = _v7.b;
		return $elm$core$Maybe$Just(
			$author$project$DataConvert$TS1E1R1U4(
				{e1: e, r1: r, t: tempo, u1: u1, u2: u2, u3: u3, u4: u4, x1: x}));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$DataConvert$toChartDataTS1E1R1U4 = $elm$core$List$filterMap($author$project$DataConvert$toChartDatumTS1E1R1U4);
var $author$project$ModelSystem$Level$runEdo = F4(
	function (model, edoParam, refFunc, controller) {
		var initState = A2(
			$elm$core$List$cons,
			function ($) {
				return $.h0;
			}(model),
			_List_Nil);
		var edoSist = $author$project$EdoSolver$Controlled(
			{
				controller: controller,
				outputFunc: $author$project$ModelSystem$Level$output,
				refFunc: refFunc,
				sistFunc: $author$project$ModelSystem$Level$system(model)
			});
		var _v0 = A3($author$project$EdoSolver$edoSolverReversed, edoParam, edoSist, initState);
		var edoData = _v0.a;
		var edoParamNew = _v0.b;
		return _Utils_Tuple2(
			$author$project$DataConvert$toChartDataTS1E1R1U4(edoData),
			edoParamNew);
	});
var $author$project$ModelSystem$Level2$output = F2(
	function (tempo, xs) {
		if (xs.b && xs.b.b) {
			var x1 = xs.a;
			var _v1 = xs.b;
			var x2 = _v1.a;
			var ls = _v1.b;
			return _List_fromArray(
				[x2]);
		} else {
			return xs;
		}
	});
var $author$project$ModelSystem$Level2$system = F4(
	function (model, us, t, state) {
		var uAux = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$head(us));
		var u = (uAux < 0.0) ? 0.0 : uAux;
		var g = 9.81;
		var ap2 = function ($) {
			return $.ap2;
		}(model);
		var ap1 = function ($) {
			return $.ap1;
		}(model);
		var ag2 = function ($) {
			return $.ag2;
		}(model);
		var ag1 = function ($) {
			return $.ag1;
		}(model);
		if (state.b && state.b.b) {
			var h1 = state.a;
			var _v1 = state.b;
			var h2 = _v1.a;
			var ls = _v1.b;
			var qi1 = u;
			var h2n = (h2 >= 0.0) ? h2 : 0.0;
			var qo2 = (ap2 * $elm$core$Basics$sqrt(2.0 * g)) * $elm$core$Basics$sqrt(h2n);
			var h1n = (h1 >= 0.0) ? h1 : 0.0;
			var qo1 = (ap1 * $elm$core$Basics$sqrt(2.0 * g)) * $elm$core$Basics$sqrt(h1n);
			var qi2 = qo1;
			return A2(
				$elm$core$List$cons,
				(qi1 / ag1) - (qo1 / ag1),
				A2($elm$core$List$cons, (qi2 / ag2) - (qo2 / ag2), _List_Nil));
		} else {
			return A2($elm$core$List$cons, 0.0, _List_Nil);
		}
	});
var $author$project$DataConvert$TS2E1R1U4 = function (a) {
	return {$: 'TS2E1R1U4', a: a};
};
var $author$project$DataConvert$toChartDatumTS2E1R1U4 = function (edoDatum) {
	var tempo = edoDatum.a;
	var sist = edoDatum.b;
	if (((((((sist.b && sist.b.b) && sist.b.b.b) && sist.b.b.b.b) && sist.b.b.b.b.b) && sist.b.b.b.b.b.b) && sist.b.b.b.b.b.b.b) && sist.b.b.b.b.b.b.b.b) {
		var x1 = sist.a;
		var _v2 = sist.b;
		var x2 = _v2.a;
		var _v3 = _v2.b;
		var e = _v3.a;
		var _v4 = _v3.b;
		var r = _v4.a;
		var _v5 = _v4.b;
		var u1 = _v5.a;
		var _v6 = _v5.b;
		var u2 = _v6.a;
		var _v7 = _v6.b;
		var u3 = _v7.a;
		var _v8 = _v7.b;
		var u4 = _v8.a;
		var xs = _v8.b;
		return $elm$core$Maybe$Just(
			$author$project$DataConvert$TS2E1R1U4(
				{e1: e, r1: r, t: tempo, u1: u1, u2: u2, u3: u3, u4: u4, x1: x1, x2: x2}));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$DataConvert$toChartDataTS2E1R1U4 = $elm$core$List$filterMap($author$project$DataConvert$toChartDatumTS2E1R1U4);
var $author$project$ModelSystem$Level2$runEdo = F4(
	function (model, edoParam, refFunc, controller) {
		var initState = A2(
			$elm$core$List$cons,
			function ($) {
				return $.h01;
			}(model),
			A2(
				$elm$core$List$cons,
				function ($) {
					return $.h02;
				}(model),
				_List_Nil));
		var edoSist = $author$project$EdoSolver$Controlled(
			{
				controller: controller,
				outputFunc: $author$project$ModelSystem$Level2$output,
				refFunc: refFunc,
				sistFunc: $author$project$ModelSystem$Level2$system(model)
			});
		var _v0 = A3($author$project$EdoSolver$edoSolverReversed, edoParam, edoSist, initState);
		var edoData = _v0.a;
		var edoParamNew = _v0.b;
		return _Utils_Tuple2(
			$author$project$DataConvert$toChartDataTS2E1R1U4(edoData),
			edoParamNew);
	});
var $author$project$ModelSystem$runEdo = F4(
	function (model, edoParam, refFunc, controller) {
		if (model.$ === 'LevelModel') {
			var levelModel = model.a;
			return A4($author$project$ModelSystem$Level$runEdo, levelModel, edoParam, refFunc, controller);
		} else {
			var level2Model = model.a;
			return A4($author$project$ModelSystem$Level2$runEdo, level2Model, edoParam, refFunc, controller);
		}
	});
var $elm$core$String$toFloat = _String_toFloat;
var $author$project$Controller$PID$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'PidKp':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var kp = function ($) {
					return $.kp;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, kp, maybeVal);
				return _Utils_update(
					model,
					{kp: val, kpStr: valueStr});
			case 'PidKi':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var ki = function ($) {
					return $.ki;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, ki, maybeVal);
				return _Utils_update(
					model,
					{ki: val, kiStr: valueStr});
			default:
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var kd = function ($) {
					return $.kd;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, kd, maybeVal);
				return _Utils_update(
					model,
					{kd: val, kdStr: valueStr});
		}
	});
var $author$project$Controller$PIDeq$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'PidKp':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var kp = function ($) {
					return $.kp;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, kp, maybeVal);
				return _Utils_update(
					model,
					{kp: val, kpStr: valueStr});
			case 'PidKi':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var ki = function ($) {
					return $.ki;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, ki, maybeVal);
				return _Utils_update(
					model,
					{ki: val, kiStr: valueStr});
			case 'PidKd':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var kd = function ($) {
					return $.kd;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, kd, maybeVal);
				return _Utils_update(
					model,
					{kd: val, kdStr: valueStr});
			default:
				var valueStr = msg.a;
				var ueq = function ($) {
					return $.ueq;
				}(model);
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var val = A2($elm$core$Maybe$withDefault, ueq, maybeVal);
				return _Utils_update(
					model,
					{ueq: val, ueqStr: valueStr});
		}
	});
var $author$project$Controller$update = F2(
	function (msg, model) {
		if (msg.$ === 'PidMsg') {
			var pidMsg = msg.a;
			if (model.$ === 'PidModel') {
				var pidModel = model.a;
				return $author$project$Controller$PidModel(
					A2($author$project$Controller$PID$update, pidMsg, pidModel));
			} else {
				return model;
			}
		} else {
			var pideqMsg = msg.a;
			if (model.$ === 'PideqModel') {
				var pideqModel = model.a;
				return $author$project$Controller$PideqModel(
					A2($author$project$Controller$PIDeq$update, pideqMsg, pideqModel));
			} else {
				return model;
			}
		}
	});
var $author$project$ModelSystem$Level$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'H0':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var h0 = function ($) {
					return $.h0;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, h0, maybeVal);
				return _Utils_update(
					model,
					{h0: val, h0Str: valueStr});
			case 'Ag':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var ag = function ($) {
					return $.ag;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, ag, maybeVal);
				return _Utils_update(
					model,
					{ag: val, agStr: valueStr});
			default:
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var ap = function ($) {
					return $.ap;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, ap, maybeVal);
				return _Utils_update(
					model,
					{ap: val, apStr: valueStr});
		}
	});
var $author$project$ModelSystem$Level2$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'H01':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var h01 = function ($) {
					return $.h01;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, h01, maybeVal);
				return _Utils_update(
					model,
					{h01: val, h01Str: valueStr});
			case 'Ag1':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var ag1 = function ($) {
					return $.ag1;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, ag1, maybeVal);
				return _Utils_update(
					model,
					{ag1: val, ag1Str: valueStr});
			case 'Ap1':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var ap1 = function ($) {
					return $.ap1;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, ap1, maybeVal);
				return _Utils_update(
					model,
					{ap1: val, ap1Str: valueStr});
			case 'H02':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var h02 = function ($) {
					return $.h02;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, h02, maybeVal);
				return _Utils_update(
					model,
					{h02: val, h02Str: valueStr});
			case 'Ag2':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var ag2 = function ($) {
					return $.ag2;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, ag2, maybeVal);
				return _Utils_update(
					model,
					{ag2: val, ag2Str: valueStr});
			default:
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var ap2 = function ($) {
					return $.ap2;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, ap2, maybeVal);
				return _Utils_update(
					model,
					{ap2: val, ap2Str: valueStr});
		}
	});
var $author$project$ModelSystem$update = F2(
	function (msg, model) {
		if (msg.$ === 'LevelMsg') {
			var levelMsg = msg.a;
			if (model.$ === 'LevelModel') {
				var levelModel = model.a;
				return $author$project$ModelSystem$LevelModel(
					A2($author$project$ModelSystem$Level$update, levelMsg, levelModel));
			} else {
				return model;
			}
		} else {
			var level2Msg = msg.a;
			if (model.$ === 'Level2Model') {
				var level2Model = model.a;
				return $author$project$ModelSystem$Level2Model(
					A2($author$project$ModelSystem$Level2$update, level2Msg, level2Model));
			} else {
				return model;
			}
		}
	});
var $author$project$MyCharts$Chart$addModel = function (chartID) {
	return {
		chartID: chartID,
		curves: _List_fromArray(
			[
				$author$project$MyCharts$Chart$Curve$init($elm$core$Maybe$Nothing)
			]),
		editingCurves: false,
		zoom: $author$project$MyCharts$Chart$myInitZoom,
		zoomModel: $author$project$MyCharts$Chart$Zoom$init
	};
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$MyCharts$Chart$lastElem = function (list) {
	lastElem:
	while (true) {
		if (!list.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!list.b.b) {
				var last = list.a;
				return $elm$core$Maybe$Just(last);
			} else {
				var head = list.a;
				var rest = list.b;
				var $temp$list = rest;
				list = $temp$list;
				continue lastElem;
			}
		}
	}
};
var $author$project$MyCharts$Chart$Curve$update = F2(
	function (msg, model) {
		var otherCurveID = function ($) {
			return $.curveID;
		}(model);
		var _v0 = function ($) {
			return $.axesString;
		}(model);
		var xAxis = _v0.a;
		var yAxis = _v0.b;
		if (msg.$ === 'XAxis') {
			var axisString = msg.a;
			return _Utils_update(
				model,
				{
					axesString: _Utils_Tuple2(axisString, yAxis)
				});
		} else {
			var axisString = msg.a;
			return _Utils_update(
				model,
				{
					axesString: _Utils_Tuple2(xAxis, axisString)
				});
		}
	});
var $author$project$MyCharts$Chart$Zoom$ReZooming = F4(
	function (a, b, c, d) {
		return {$: 'ReZooming', a: a, b: b, c: c, d: d};
	});
var $author$project$MyCharts$Chart$Zoom$Zoomed = F2(
	function (a, b) {
		return {$: 'Zoomed', a: a, b: b};
	});
var $author$project$MyCharts$Chart$Zoom$Zooming = F2(
	function (a, b) {
		return {$: 'Zooming', a: a, b: b};
	});
var $author$project$MyCharts$Chart$Zoom$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'OnDown':
				var point = msg.a;
				switch (model.$) {
					case 'NoZoom':
						return A2($author$project$MyCharts$Chart$Zoom$Zooming, point, point);
					case 'Zoomed':
						var start = model.a;
						var end = model.b;
						return A4($author$project$MyCharts$Chart$Zoom$ReZooming, start, end, point, point);
					default:
						return model;
				}
			case 'OnMove':
				var point = msg.a;
				switch (model.$) {
					case 'Zooming':
						var start = model.a;
						return A2($author$project$MyCharts$Chart$Zoom$Zooming, start, point);
					case 'ReZooming':
						var startOld = model.a;
						var endOld = model.b;
						var start = model.c;
						return A4($author$project$MyCharts$Chart$Zoom$ReZooming, startOld, endOld, start, point);
					default:
						return model;
				}
			case 'OnUp':
				var point = msg.a;
				switch (model.$) {
					case 'Zooming':
						var start = model.a;
						return A2(
							$author$project$MyCharts$Chart$Zoom$Zoomed,
							{
								x: A2($elm$core$Basics$min, start.x, point.x),
								y: A2($elm$core$Basics$min, start.y, point.y)
							},
							{
								x: A2($elm$core$Basics$max, start.x, point.x),
								y: A2($elm$core$Basics$max, start.y, point.y)
							});
					case 'ReZooming':
						var start = model.c;
						return A2(
							$author$project$MyCharts$Chart$Zoom$Zoomed,
							{
								x: A2($elm$core$Basics$min, start.x, point.x),
								y: A2($elm$core$Basics$min, start.y, point.y)
							},
							{
								x: A2($elm$core$Basics$max, start.x, point.x),
								y: A2($elm$core$Basics$max, start.y, point.y)
							});
					default:
						return model;
				}
			default:
				return $author$project$MyCharts$Chart$Zoom$NoZoom;
		}
	});
var $gampleman$elm_visualization$Zoom$OneFinger = function (a) {
	return {$: 'OneFinger', a: a};
};
var $gampleman$elm_visualization$Zoom$TwoFingers = F2(
	function (a, b) {
		return {$: 'TwoFingers', a: a, b: b};
	});
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $gampleman$elm_visualization$Zoom$Transform$invert = F2(
	function (_v0, _v1) {
		var locX = _v0.a;
		var locY = _v0.b;
		var k = _v1.k;
		var x = _v1.x;
		var y = _v1.y;
		return _Utils_Tuple2((locX - x) / k, (locY - y) / k);
	});
var $gampleman$elm_visualization$Zoom$Transform$translate = F2(
	function (_v0, _v1) {
		var locX = _v0.a;
		var locY = _v0.b;
		var k = _v1.k;
		var x = _v1.x;
		var y = _v1.y;
		return A3($gampleman$elm_visualization$Zoom$Transform$Transform, k, x + (k * locX), y + (k * locY));
	});
var $gampleman$elm_visualization$Zoom$constrain = F3(
	function (_v0, _v1, tsfm) {
		var extentT = _v0.a;
		var extentB = _v0.b;
		var translateExtentT = _v1.a;
		var translateExtentB = _v1.b;
		var minus = F2(
			function (_v5, _v6) {
				var a = _v5.a;
				var b = _v5.b;
				var c = _v6.a;
				var d = _v6.b;
				return _Utils_Tuple2(c - a, d - b);
			});
		var _v2 = A2(
			minus,
			translateExtentB,
			A2($gampleman$elm_visualization$Zoom$Transform$invert, extentB, tsfm));
		var dx1 = _v2.a;
		var dy1 = _v2.b;
		var _v3 = A2(
			minus,
			translateExtentT,
			A2($gampleman$elm_visualization$Zoom$Transform$invert, extentT, tsfm));
		var dx0 = _v3.a;
		var dy0 = _v3.b;
		var _v4 = _Utils_Tuple2(
			A2($elm$core$Basics$min, 0, dx0),
			A2($elm$core$Basics$min, 0, dy0));
		var minDX = _v4.a;
		var minDY = _v4.b;
		return A2(
			$gampleman$elm_visualization$Zoom$Transform$translate,
			_Utils_Tuple2(
				(_Utils_cmp(dx1, dx0) > 0) ? ((dx0 + dx1) / 2) : ((!minDX) ? A2($elm$core$Basics$max, 0, dx1) : minDX),
				(_Utils_cmp(dy1, dy0) > 0) ? ((dy0 + dy1) / 2) : ((!minDY) ? A2($elm$core$Basics$max, 0, dy1) : minDY)),
			tsfm);
	});
var $elm$core$Basics$pow = _Basics_pow;
var $gampleman$elm_visualization$Zoom$dist2 = F2(
	function (_v0, _v1) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var x2 = _v1.a;
		var y2 = _v1.b;
		return A2($elm$core$Basics$pow, x2 - x1, 2) + A2($elm$core$Basics$pow, y2 - y1, 2);
	});
var $gampleman$elm_visualization$Zoom$easingInOutCubic = function (t) {
	return (t < 0.5) ? (A2($elm$core$Basics$pow, t * 2, 3) / 2) : (1 - (0.5 * A2($elm$core$Basics$pow, ((-2) * t) + 2, 3)));
};
var $gampleman$elm_visualization$Zoom$findAssociatedTouch = function (_v0) {
	var identifier = _v0.identifier;
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$List$filter(
			function (t) {
				return _Utils_eq(t.identifier, identifier);
			}),
		$elm$core$List$head);
};
var $gampleman$elm_visualization$Zoom$midpoint = F2(
	function (_v0, _v1) {
		var x1 = _v0.a;
		var y1 = _v0.b;
		var x2 = _v1.a;
		var y2 = _v1.b;
		return _Utils_Tuple2((x1 + x2) / 2, (y1 + y2) / 2);
	});
var $gampleman$elm_visualization$Zoom$Matrix$transform = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v0.b;
		var _v2 = _v1.a;
		var a11 = _v2.a;
		var a12 = _v2.b;
		var a13 = _v2.c;
		var _v3 = _v1.b;
		var a21 = _v3.a;
		var a22 = _v3.b;
		var a23 = _v3.c;
		return _Utils_Tuple2(((a11 * x) + (a12 * y)) + (a13 * 1), ((a21 * x) + (a22 * y)) + (a23 * 1));
	});
var $gampleman$elm_visualization$Events$normalizePointerPosition = F2(
	function (position, maybeMatrix) {
		if (maybeMatrix.$ === 'Just') {
			var matrix = maybeMatrix.a;
			return A2($gampleman$elm_visualization$Zoom$Matrix$transform, position, matrix);
		} else {
			return position;
		}
	});
var $gampleman$elm_visualization$Zoom$scale = F3(
	function (_v0, k, trfm) {
		var mi = _v0.a;
		var mx = _v0.b;
		return _Utils_update(
			trfm,
			{
				k: A3($elm$core$Basics$clamp, mi, mx, k)
			});
	});
var $gampleman$elm_visualization$Zoom$Transform$scale = F2(
	function (k_, _v0) {
		var k = _v0.k;
		var x = _v0.x;
		var y = _v0.y;
		return A3($gampleman$elm_visualization$Zoom$Transform$Transform, k * k_, x, y);
	});
var $elm$core$Basics$e = _Basics_e;
var $gampleman$elm_visualization$Zoom$Interpolation$exp = function (x) {
	return A2($elm$core$Basics$pow, $elm$core$Basics$e, x);
};
var $gampleman$elm_visualization$Zoom$Interpolation$cosh = function (x) {
	var x_ = $gampleman$elm_visualization$Zoom$Interpolation$exp(x);
	return (x_ + (1 / x_)) / 2;
};
var $gampleman$elm_visualization$Zoom$Interpolation$epsilon2 = 1.0e-12;
var $gampleman$elm_visualization$Zoom$Interpolation$log = $elm$core$Basics$logBase($elm$core$Basics$e);
var $gampleman$elm_visualization$Zoom$Interpolation$rho = $elm$core$Basics$sqrt(2);
var $gampleman$elm_visualization$Zoom$Interpolation$sinh = function (x) {
	var x_ = $gampleman$elm_visualization$Zoom$Interpolation$exp(x);
	return (x_ - (1 / x_)) / 2;
};
var $gampleman$elm_visualization$Zoom$Interpolation$tanh = function (x) {
	var x_ = $gampleman$elm_visualization$Zoom$Interpolation$exp(2 * x);
	return (x_ - 1) / (x_ + 1);
};
var $gampleman$elm_visualization$Zoom$Interpolation$interpolate = F2(
	function (a, b) {
		var dy = b.cy - a.cy;
		var dx = b.cx - a.cx;
		var d2 = A2($elm$core$Basics$pow, dx, 2) + A2($elm$core$Basics$pow, dy, 2);
		if (_Utils_cmp(d2, $gampleman$elm_visualization$Zoom$Interpolation$epsilon2) < 0) {
			var s = $gampleman$elm_visualization$Zoom$Interpolation$log(b.size / a.size) / $gampleman$elm_visualization$Zoom$Interpolation$rho;
			return _Utils_Tuple2(
				$elm$core$Basics$abs(s) * 1000,
				function (t) {
					return {
						cx: a.cx + (t * dx),
						cy: a.cy + (t * dy),
						size: a.size * $gampleman$elm_visualization$Zoom$Interpolation$exp(($gampleman$elm_visualization$Zoom$Interpolation$rho * t) * s)
					};
				});
		} else {
			var d1 = $elm$core$Basics$sqrt(d2);
			var b1 = ((A2($elm$core$Basics$pow, b.size, 2) - A2($elm$core$Basics$pow, a.size, 2)) - (A2($elm$core$Basics$pow, $gampleman$elm_visualization$Zoom$Interpolation$rho, 4) * d2)) / (((2 * b.size) * A2($elm$core$Basics$pow, $gampleman$elm_visualization$Zoom$Interpolation$rho, 2)) * d1);
			var r1 = $gampleman$elm_visualization$Zoom$Interpolation$log(
				$elm$core$Basics$sqrt(
					A2($elm$core$Basics$pow, b1, 2) + 1) - b1);
			var b0 = ((A2($elm$core$Basics$pow, b.size, 2) - A2($elm$core$Basics$pow, a.size, 2)) + (A2($elm$core$Basics$pow, $gampleman$elm_visualization$Zoom$Interpolation$rho, 4) * d2)) / (((2 * a.size) * A2($elm$core$Basics$pow, $gampleman$elm_visualization$Zoom$Interpolation$rho, 2)) * d1);
			var r0 = $gampleman$elm_visualization$Zoom$Interpolation$log(
				$elm$core$Basics$sqrt(
					A2($elm$core$Basics$pow, b0, 2) + 1) - b0);
			var s_ = (r1 - r0) / $gampleman$elm_visualization$Zoom$Interpolation$rho;
			return _Utils_Tuple2(
				s_ * 1000,
				function (t) {
					var s = t * s_;
					var coshr0 = $gampleman$elm_visualization$Zoom$Interpolation$cosh(r0);
					var u = (a.size / (A2($elm$core$Basics$pow, $gampleman$elm_visualization$Zoom$Interpolation$rho, 2) * d1)) * ((coshr0 * $gampleman$elm_visualization$Zoom$Interpolation$tanh(($gampleman$elm_visualization$Zoom$Interpolation$rho * s) + r0)) - $gampleman$elm_visualization$Zoom$Interpolation$sinh(r0));
					return {
						cx: a.cx + (u * dx),
						cy: a.cy + (u * dy),
						size: (a.size * coshr0) / $gampleman$elm_visualization$Zoom$Interpolation$cosh(($gampleman$elm_visualization$Zoom$Interpolation$rho * s) + r0)
					};
				});
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $gampleman$elm_visualization$Zoom$schedule = F3(
	function (btransform, center, _v0) {
		var model = _v0.a;
		var atransform = model.transform;
		var _v1 = model.extent;
		var _v2 = _v1.a;
		var top = _v2.a;
		var left = _v2.b;
		var _v3 = _v1.b;
		var bottom = _v3.a;
		var right = _v3.b;
		var w = A2($elm$core$Basics$max, bottom - top, right - left);
		var _v4 = A2($gampleman$elm_visualization$Zoom$Transform$invert, center, btransform);
		var bx = _v4.a;
		var by = _v4.b;
		var _v5 = A2($gampleman$elm_visualization$Zoom$Transform$invert, center, atransform);
		var ax = _v5.a;
		var ay = _v5.b;
		var _v6 = A2(
			$gampleman$elm_visualization$Zoom$Interpolation$interpolate,
			{cx: ax, cy: ay, size: w / atransform.k},
			{cx: bx, cy: by, size: w / btransform.k});
		var dur = _v6.a;
		var interp = _v6.b;
		return $gampleman$elm_visualization$Zoom$Zoom(
			_Utils_update(
				model,
				{
					transition: $elm$core$Maybe$Just(
						{
							duration: dur,
							elapsed: 0,
							interpolation: function (t) {
								if (t === 1) {
									return btransform;
								} else {
									var l = interp(t);
									var k = w / l.size;
									return {k: k, x: center.a - (l.cx * k), y: center.b - (l.cy * k)};
								}
							}
						})
				}));
	});
var $gampleman$elm_visualization$Zoom$translate = F3(
	function (_v0, _v1, trfm) {
		var position0x = _v0.a;
		var position0y = _v0.b;
		var position1x = _v1.a;
		var position1y = _v1.b;
		return _Utils_update(
			trfm,
			{x: position0x - (position1x * trfm.k), y: position0y - (position1y * trfm.k)});
	});
var $gampleman$elm_visualization$Zoom$update = F2(
	function (msg, _v0) {
		var model = _v0.a;
		switch (msg.$) {
			case 'DoubleClicked':
				var shiftKey = msg.a;
				var position0 = msg.b;
				var position1 = A2($gampleman$elm_visualization$Zoom$Transform$invert, position0, model.transform);
				var k = model.transform.k * (shiftKey ? 0.5 : 2);
				var newTransform = A3(
					$gampleman$elm_visualization$Zoom$constrain,
					model.extent,
					model.translateExtent,
					A3(
						$gampleman$elm_visualization$Zoom$translate,
						position0,
						position1,
						A3($gampleman$elm_visualization$Zoom$scale, model.scaleExtent, k, model.transform)));
				return A3(
					$gampleman$elm_visualization$Zoom$schedule,
					newTransform,
					position0,
					$gampleman$elm_visualization$Zoom$Zoom(
						_Utils_update(
							model,
							{touchStarted: false})));
			case 'MouseDown':
				var position = msg.a;
				var matrix = msg.b;
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{
							drag: $elm$core$Maybe$Just(
								{
									current: A2(
										$gampleman$elm_visualization$Zoom$Transform$invert,
										A2($gampleman$elm_visualization$Events$normalizePointerPosition, position, matrix),
										model.transform),
									matrix: matrix
								}),
							transition: $elm$core$Maybe$Nothing
						}));
			case 'MouseMove':
				var position_ = msg.a;
				var _v2 = model.drag;
				if (_v2.$ === 'Just') {
					var drag = _v2.a;
					var position = A2($gampleman$elm_visualization$Events$normalizePointerPosition, position_, drag.matrix);
					var trasform_ = A3($gampleman$elm_visualization$Zoom$translate, position, drag.current, model.transform);
					return $gampleman$elm_visualization$Zoom$Zoom(
						_Utils_update(
							model,
							{
								drag: $elm$core$Maybe$Just(
									_Utils_update(
										drag,
										{
											current: A2($gampleman$elm_visualization$Zoom$Transform$invert, position, trasform_)
										})),
								transform: A3($gampleman$elm_visualization$Zoom$constrain, model.extent, model.translateExtent, trasform_),
								transition: $elm$core$Maybe$Nothing
							}));
				} else {
					return $gampleman$elm_visualization$Zoom$Zoom(model);
				}
			case 'MouseUp':
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{drag: $elm$core$Maybe$Nothing}));
			case 'Wheeled':
				var delta = msg.a;
				var position0 = msg.b;
				var position1 = A2($gampleman$elm_visualization$Zoom$Transform$invert, position0, model.transform);
				var k = model.transform.k * A2($elm$core$Basics$pow, 2, delta);
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{
							transform: A3(
								$gampleman$elm_visualization$Zoom$constrain,
								model.extent,
								model.translateExtent,
								A3(
									$gampleman$elm_visualization$Zoom$translate,
									position0,
									position1,
									A3($gampleman$elm_visualization$Zoom$scale, model.scaleExtent, k, model.transform))),
							transition: $elm$core$Maybe$Nothing
						}));
			case 'GestureStarted':
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{gestureLastScale: 1}));
			case 'Gestured':
				var scl = msg.a;
				var position0 = msg.b;
				var position1 = A2($gampleman$elm_visualization$Zoom$Transform$invert, position0, model.transform);
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{
							gestureLastScale: scl,
							transform: A3(
								$gampleman$elm_visualization$Zoom$constrain,
								model.extent,
								model.translateExtent,
								A3(
									$gampleman$elm_visualization$Zoom$translate,
									position0,
									position1,
									A2($gampleman$elm_visualization$Zoom$Transform$scale, scl / model.gestureLastScale, model.transform))),
							transition: $elm$core$Maybe$Nothing
						}));
			case 'TouchStartedExpired':
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{touchStarted: false}));
			case 'TouchStarted':
				var touches = msg.a;
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{
							touchStarted: true,
							touches: function () {
								var _v3 = _Utils_Tuple2(model.touches, touches);
								_v3$3:
								while (true) {
									if (_v3.b.b) {
										switch (_v3.a.$) {
											case 'NoTouches':
												if (!_v3.b.b.b) {
													var _v4 = _v3.a;
													var _v5 = _v3.b;
													var single = _v5.a;
													return $gampleman$elm_visualization$Zoom$OneFinger(
														{
															identifier: single.identifier,
															position: single.position,
															previous: A2($gampleman$elm_visualization$Zoom$Transform$invert, single.position, model.transform)
														});
												} else {
													var _v6 = _v3.a;
													var _v7 = _v3.b;
													var one = _v7.a;
													var _v8 = _v7.b;
													var two = _v8.a;
													return A2(
														$gampleman$elm_visualization$Zoom$TwoFingers,
														{
															identifier: one.identifier,
															position: one.position,
															previous: A2($gampleman$elm_visualization$Zoom$Transform$invert, one.position, model.transform)
														},
														{
															identifier: two.identifier,
															position: two.position,
															previous: A2($gampleman$elm_visualization$Zoom$Transform$invert, two.position, model.transform)
														});
												}
											case 'OneFinger':
												var id1 = _v3.a.a;
												var _v9 = _v3.b;
												var one = _v9.a;
												return A2(
													$gampleman$elm_visualization$Zoom$TwoFingers,
													id1,
													{
														identifier: one.identifier,
														position: one.position,
														previous: A2($gampleman$elm_visualization$Zoom$Transform$invert, one.position, model.transform)
													});
											default:
												break _v3$3;
										}
									} else {
										break _v3$3;
									}
								}
								return model.touches;
							}(),
							transition: $elm$core$Maybe$Nothing
						}));
			case 'TouchMoved':
				var touches = msg.a;
				return $gampleman$elm_visualization$Zoom$Zoom(
					function () {
						var _v10 = model.touches;
						switch (_v10.$) {
							case 'OneFinger':
								var before = _v10.a;
								var _v11 = A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, before, touches);
								if (_v11.$ === 'Just') {
									var after = _v11.a;
									return _Utils_update(
										model,
										{
											touchStarted: false,
											touches: $gampleman$elm_visualization$Zoom$OneFinger(
												_Utils_update(
													before,
													{position: after.position})),
											transform: A3(
												$gampleman$elm_visualization$Zoom$constrain,
												model.extent,
												model.translateExtent,
												A3($gampleman$elm_visualization$Zoom$translate, after.position, before.previous, model.transform)),
											transition: $elm$core$Maybe$Nothing
										});
								} else {
									return model;
								}
							case 'TwoFingers':
								var beforeTouch1 = _v10.a;
								var beforeTouch2 = _v10.b;
								var t = model.transform;
								var before2 = beforeTouch2.previous;
								var before1 = beforeTouch1.previous;
								var distBefore = A2($gampleman$elm_visualization$Zoom$dist2, before1, before2);
								var after2 = A2(
									$elm$core$Maybe$withDefault,
									beforeTouch2.position,
									A2(
										$elm$core$Maybe$map,
										function ($) {
											return $.position;
										},
										A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, beforeTouch2, touches)));
								var after1 = A2(
									$elm$core$Maybe$withDefault,
									beforeTouch1.position,
									A2(
										$elm$core$Maybe$map,
										function ($) {
											return $.position;
										},
										A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, beforeTouch1, touches)));
								var distAfter = A2($gampleman$elm_visualization$Zoom$dist2, after1, after2);
								return _Utils_update(
									model,
									{
										touchStarted: false,
										touches: A2(
											$gampleman$elm_visualization$Zoom$TwoFingers,
											_Utils_update(
												beforeTouch1,
												{position: after1}),
											_Utils_update(
												beforeTouch2,
												{position: after2})),
										transform: A3(
											$gampleman$elm_visualization$Zoom$constrain,
											model.extent,
											model.translateExtent,
											A3(
												$gampleman$elm_visualization$Zoom$translate,
												A2($gampleman$elm_visualization$Zoom$midpoint, after1, after2),
												A2($gampleman$elm_visualization$Zoom$midpoint, before1, before2),
												A3(
													$gampleman$elm_visualization$Zoom$scale,
													model.scaleExtent,
													$elm$core$Basics$sqrt(distAfter / distBefore),
													t))),
										transition: $elm$core$Maybe$Nothing
									});
							default:
								return model;
						}
					}());
			case 'TouchEnded':
				var touches = msg.a;
				return $gampleman$elm_visualization$Zoom$Zoom(
					_Utils_update(
						model,
						{
							touches: function () {
								var _v12 = model.touches;
								switch (_v12.$) {
									case 'NoTouches':
										return $gampleman$elm_visualization$Zoom$NoTouches;
									case 'OneFinger':
										var touch1 = _v12.a;
										var _v13 = A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, touch1, touches);
										if (_v13.$ === 'Just') {
											return $gampleman$elm_visualization$Zoom$NoTouches;
										} else {
											return model.touches;
										}
									default:
										var before1 = _v12.a;
										var before2 = _v12.b;
										var _v14 = _Utils_Tuple2(
											A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, before1, touches),
											A2($gampleman$elm_visualization$Zoom$findAssociatedTouch, before2, touches));
										if (_v14.a.$ === 'Just') {
											if (_v14.b.$ === 'Just') {
												return $gampleman$elm_visualization$Zoom$NoTouches;
											} else {
												var _v15 = _v14.b;
												return $gampleman$elm_visualization$Zoom$OneFinger(before2);
											}
										} else {
											if (_v14.b.$ === 'Just') {
												var _v16 = _v14.a;
												return $gampleman$elm_visualization$Zoom$OneFinger(before1);
											} else {
												var _v17 = _v14.a;
												var _v18 = _v14.b;
												return model.touches;
											}
										}
								}
							}()
						}));
			default:
				var delta = msg.a;
				var _v19 = model.transition;
				if (_v19.$ === 'Just') {
					var transition = _v19.a;
					var duration = transition.duration;
					var elapsed = transition.elapsed;
					var interpolation = transition.interpolation;
					return $gampleman$elm_visualization$Zoom$Zoom(
						_Utils_update(
							model,
							{
								transform: interpolation(
									$gampleman$elm_visualization$Zoom$easingInOutCubic(
										A3($elm$core$Basics$clamp, 0, 1, (elapsed + delta) / duration))),
								transition: (_Utils_cmp(elapsed + delta, duration) > -1) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
									_Utils_update(
										transition,
										{elapsed: elapsed + delta}))
							}));
				} else {
					return $gampleman$elm_visualization$Zoom$Zoom(model);
				}
		}
	});
var $author$project$MyCharts$Chart$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'CurveMsg':
				var curveID = msg.a;
				var curveMsg = msg.b;
				var curves = function ($) {
					return $.curves;
				}(model);
				var newCurves = A2(
					$elm$core$List$map,
					function (curve) {
						return _Utils_eq(curve.curveID, curveID) ? A2($author$project$MyCharts$Chart$Curve$update, curveMsg, curve) : curve;
					},
					curves);
				return _Utils_update(
					model,
					{curves: newCurves});
			case 'AddCurve':
				var curves = function ($) {
					return $.curves;
				}(model);
				var maybeLastCurve = $author$project$MyCharts$Chart$lastElem(curves);
				var newCurve = $author$project$MyCharts$Chart$Curve$init(maybeLastCurve);
				var newCurves = _Utils_ap(
					curves,
					A2($elm$core$List$cons, newCurve, _List_Nil));
				return _Utils_update(
					model,
					{curves: newCurves});
			case 'RemoveCurve':
				var curveID = msg.a;
				var curves = function ($) {
					return $.curves;
				}(model);
				var newCurves = A2(
					$elm$core$List$filter,
					function (c) {
						return _Utils_eq(
							function ($) {
								return $.curveID;
							}(c),
							curveID) ? false : true;
					},
					curves);
				return _Utils_update(
					model,
					{curves: newCurves});
			case 'ToggleEditCurve':
				var editingCurves = function ($) {
					return $.editingCurves;
				}(model);
				return _Utils_update(
					model,
					{editingCurves: !editingCurves});
			case 'ZoomMsg':
				var zoomMsg = msg.a;
				var zoomModel = function ($) {
					return $.zoomModel;
				}(model);
				var newZoomModel = A2($author$project$MyCharts$Chart$Zoom$update, zoomMsg, zoomModel);
				return _Utils_update(
					model,
					{zoomModel: newZoomModel});
			default:
				var evZoomMsg = msg.a;
				return _Utils_update(
					model,
					{
						zoom: A2($gampleman$elm_visualization$Zoom$update, evZoomMsg, model.zoom)
					});
		}
	});
var $author$project$MyCharts$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'AddChart':
				var chartID = msg.a;
				var newChartParam = $author$project$MyCharts$Chart$addModel(chartID);
				var newChartsParam = A2($elm$core$List$cons, newChartParam, model);
				return A2(
					$elm$core$List$sortBy,
					function (cp) {
						return cp.chartID;
					},
					newChartsParam);
			case 'RemoveChart':
				var chartID = msg.a;
				return A2(
					$elm$core$List$filter,
					function (chartModel) {
						return !_Utils_eq(
							function ($) {
								return $.chartID;
							}(chartModel),
							chartID);
					},
					model);
			default:
				var chartID = msg.a;
				var chartMsg = msg.b;
				return A2(
					$elm$core$List$map,
					function (chartModel) {
						return _Utils_eq(chartModel.chartID, chartID) ? A2($author$project$MyCharts$Chart$update, chartMsg, chartModel) : chartModel;
					},
					model);
		}
	});
var $author$project$Reference$Step1$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Step1IVal':
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var iVal = function ($) {
					return $.iVal;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, iVal, maybeVal);
				return _Utils_update(
					model,
					{iVal: val, iValStr: valueStr});
			case 'Step1TStep':
				var valueStr = msg.a;
				var tStep = function ($) {
					return $.tStep;
				}(model);
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var val = A2($elm$core$Maybe$withDefault, tStep, maybeVal);
				return _Utils_update(
					model,
					{tStep: val, tStepStr: valueStr});
			default:
				var valueStr = msg.a;
				var maybeVal = $elm$core$String$toFloat(valueStr);
				var fVal = function ($) {
					return $.fVal;
				}(model);
				var val = A2($elm$core$Maybe$withDefault, fVal, maybeVal);
				return _Utils_update(
					model,
					{fVal: val, fValStr: valueStr});
		}
	});
var $author$project$Reference$update = F2(
	function (msg, model) {
		var step1Msg = msg.a;
		var step1Model = model.a;
		return $author$project$Reference$Step1Model(
			A2($author$project$Reference$Step1$update, step1Msg, step1Model));
	});
var $author$project$EdoSolver$updateEdoIStates = F2(
	function (msg, edoIStates) {
		if (msg.$ === 'Tini') {
			var valueStr = msg.a;
			return _Utils_update(
				edoIStates,
				{tiniStr: valueStr});
		} else {
			var valueStr = msg.a;
			return _Utils_update(
				edoIStates,
				{tfimStr: valueStr});
		}
	});
var $author$project$EdoSolver$updateEdoParam = F2(
	function (edoParam, edoIStates) {
		var tiniStr = function ($) {
			return $.tiniStr;
		}(edoIStates);
		var tfimStr = function ($) {
			return $.tfimStr;
		}(edoIStates);
		var listStr = _List_fromArray(
			[tiniStr, tfimStr]);
		var listValues = A2($elm$core$List$filterMap, $elm$core$String$toFloat, listStr);
		if ((listValues.b && listValues.b.b) && (!listValues.b.b.b)) {
			var tini = listValues.a;
			var _v1 = listValues.b;
			var tfim = _v1.a;
			return _Utils_update(
				edoParam,
				{tempo: tini, tfim: tfim});
		} else {
			return edoParam;
		}
	});
var $author$project$ModelSystem$Level$updateModelFromXs = F2(
	function (xs, model) {
		var h0 = function ($) {
			return $.h0;
		}(model);
		var newH0 = A2(
			$elm$core$Maybe$withDefault,
			h0,
			$elm$core$List$head(xs));
		return _Utils_update(
			model,
			{h0: newH0});
	});
var $author$project$ModelSystem$Level2$updateModelFromXs = F2(
	function (xs, model) {
		var h02 = function ($) {
			return $.h02;
		}(model);
		var h01 = function ($) {
			return $.h01;
		}(model);
		var _v0 = function () {
			if ((xs.b && xs.b.b) && (!xs.b.b.b)) {
				var xs1 = xs.a;
				var _v2 = xs.b;
				var xs2 = _v2.a;
				return _Utils_Tuple2(xs1, xs2);
			} else {
				return _Utils_Tuple2(0.0, 0.0);
			}
		}();
		var newH01 = _v0.a;
		var newH02 = _v0.b;
		return _Utils_update(
			model,
			{h01: newH01, h02: newH02});
	});
var $author$project$ModelSystem$updateModelFromXs = F2(
	function (xs, model) {
		if (model.$ === 'LevelModel') {
			var levelModel = model.a;
			return $author$project$ModelSystem$LevelModel(
				A2($author$project$ModelSystem$Level$updateModelFromXs, xs, levelModel));
		} else {
			var level2Model = model.a;
			return $author$project$ModelSystem$Level2Model(
				A2($author$project$ModelSystem$Level2$updateModelFromXs, xs, level2Model));
		}
	});
var $author$project$Main$updatingBigModelFromModel = F2(
	function (bigModel, newModel) {
		if (bigModel.$ === 'SolvingEdo') {
			return $author$project$Main$SolvingEdo(newModel);
		} else {
			var animatingEdoParam = bigModel.b;
			var xs = bigModel.c;
			return A3($author$project$Main$Animation, newModel, animatingEdoParam, xs);
		}
	});
var $author$project$DataConvert$xsFromDatum = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return _List_fromArray(
				[
					function ($) {
					return $.x1;
				}(datum)
				]);
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return _List_fromArray(
				[
					function ($) {
					return $.x1;
				}(datum)
				]);
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return _List_fromArray(
				[
					function ($) {
					return $.x1;
				}(datum)
				]);
		default:
			var datum = chartDatum.a;
			return _List_fromArray(
				[
					function ($) {
					return $.x1;
				}(datum),
					function ($) {
					return $.x2;
				}(datum)
				]);
	}
};
var $author$project$ModelSystem$Level$xsFromModel = function (model) {
	return _List_fromArray(
		[
			function ($) {
			return $.h0;
		}(model)
		]);
};
var $author$project$ModelSystem$Level2$xsFromModel = function (model) {
	return _List_fromArray(
		[
			function ($) {
			return $.h01;
		}(model),
			function ($) {
			return $.h02;
		}(model)
		]);
};
var $author$project$ModelSystem$xsFromModel = function (model) {
	if (model.$ === 'LevelModel') {
		var levelModel = model.a;
		return $author$project$ModelSystem$Level$xsFromModel(levelModel);
	} else {
		var level2Model = model.a;
		return $author$project$ModelSystem$Level2$xsFromModel(level2Model);
	}
};
var $author$project$Main$update = F2(
	function (msg, bigModel) {
		var model = $author$project$Main$modelFromBigModel(bigModel);
		switch (msg.$) {
			case 'EdoMsg':
				var edoMsg = msg.a;
				var edoIStates = function ($) {
					return $.edoIStates;
				}(model);
				var edoIStatesNew = A2($author$project$EdoSolver$updateEdoIStates, edoMsg, edoIStates);
				var newModel = _Utils_update(
					model,
					{edoIStates: edoIStatesNew});
				return _Utils_Tuple2(
					A2($author$project$Main$updatingBigModelFromModel, bigModel, newModel),
					$elm$core$Platform$Cmd$none);
			case 'UpdateEdoParam':
				var edoParam = function ($) {
					return $.edoParam;
				}(model);
				var edoIStates = function ($) {
					return $.edoIStates;
				}(model);
				var edoParamNew = A2($author$project$EdoSolver$updateEdoParam, edoParam, edoIStates);
				var newModel = _Utils_update(
					model,
					{edoParam: edoParamNew});
				return _Utils_Tuple2(
					A2($author$project$Main$updatingBigModelFromModel, bigModel, newModel),
					$elm$core$Platform$Cmd$none);
			case 'ModelSystemMsg':
				var modelSystemMsg = msg.a;
				var modelSystemModel = function ($) {
					return $.modelSystemModel;
				}(model);
				var modelSystemModelNew = A2($author$project$ModelSystem$update, modelSystemMsg, modelSystemModel);
				var newModel = _Utils_update(
					model,
					{modelSystemModel: modelSystemModelNew});
				return _Utils_Tuple2(
					A2($author$project$Main$updatingBigModelFromModel, bigModel, newModel),
					$elm$core$Platform$Cmd$none);
			case 'ControlMsg':
				var controlMsg = msg.a;
				var controlModel = function ($) {
					return $.controlModel;
				}(model);
				var controlModelNew = A2($author$project$Controller$update, controlMsg, controlModel);
				var newModel = _Utils_update(
					model,
					{controlModel: controlModelNew});
				return _Utils_Tuple2(
					A2($author$project$Main$updatingBigModelFromModel, bigModel, newModel),
					$elm$core$Platform$Cmd$none);
			case 'RefMsg':
				var refMsg = msg.a;
				var refModel = function ($) {
					return $.refModel;
				}(model);
				var refModelNew = A2($author$project$Reference$update, refMsg, refModel);
				var newModel = _Utils_update(
					model,
					{refModel: refModelNew});
				return _Utils_Tuple2(
					A2($author$project$Main$updatingBigModelFromModel, bigModel, newModel),
					$elm$core$Platform$Cmd$none);
			case 'MyChartsMsg':
				var chartsMsg = msg.a;
				var chartsModel = function ($) {
					return $.chartsModel;
				}(model);
				var newChartsModel = A2($author$project$MyCharts$update, chartsMsg, chartsModel);
				var newModel = _Utils_update(
					model,
					{chartsModel: newChartsModel});
				return _Utils_Tuple2(
					A2($author$project$Main$updatingBigModelFromModel, bigModel, newModel),
					$elm$core$Platform$Cmd$none);
			case 'RunEdo':
				var newModel = $author$project$Main$modelFromBigModel(
					A2($author$project$Main$update, $author$project$Main$UpdateEdoParam, bigModel).a);
				var refModel = function ($) {
					return $.refModel;
				}(newModel);
				var refFunc = $author$project$Reference$refFunctionFromModel(refModel);
				var modelSystemModel = function ($) {
					return $.modelSystemModel;
				}(newModel);
				var edoParam = function ($) {
					return $.edoParam;
				}(newModel);
				var controlModel = function ($) {
					return $.controlModel;
				}(newModel);
				var controller = $author$project$Controller$controllerFromModel(controlModel);
				var controlMem = _List_Nil;
				var newEdoParam = _Utils_update(
					edoParam,
					{controlMemory: controlMem});
				var data = A4($author$project$ModelSystem$runEdo, modelSystemModel, newEdoParam, refFunc, controller).a;
				return _Utils_Tuple2(
					$author$project$Main$SolvingEdo(
						_Utils_update(
							newModel,
							{chartData: data, edoParam: newEdoParam})),
					$elm$core$Platform$Cmd$none);
			case 'RunAnimation':
				var updatedModel = $author$project$Main$modelFromBigModel(
					A2($author$project$Main$update, $author$project$Main$UpdateEdoParam, bigModel).a);
				var modelSystemModel = function ($) {
					return $.modelSystemModel;
				}(updatedModel);
				var xs = $author$project$ModelSystem$xsFromModel(modelSystemModel);
				var edoParam = function ($) {
					return $.edoParam;
				}(updatedModel);
				var tiniAnimation = function ($) {
					return $.tempo;
				}(edoParam);
				var controlMem = _List_Nil;
				var chartData = _List_Nil;
				var newModel = _Utils_update(
					updatedModel,
					{chartData: chartData});
				var animatingEdoParam = _Utils_update(
					edoParam,
					{controlMemory: controlMem, tfim: tiniAnimation});
				return _Utils_Tuple2(
					A3($author$project$Main$Animation, newModel, animatingEdoParam, xs),
					$elm$core$Platform$Cmd$none);
			default:
				var dTime = msg.a;
				if (bigModel.$ === 'SolvingEdo') {
					return _Utils_Tuple2(bigModel, $elm$core$Platform$Cmd$none);
				} else {
					var modelAnimation = bigModel.a;
					var animatingEdoParam = bigModel.b;
					var xs = bigModel.c;
					var tempo = function ($) {
						return $.tempo;
					}(animatingEdoParam);
					var modelEdoParam = function ($) {
						return $.edoParam;
					}(modelAnimation);
					var tfimAnimation = function ($) {
						return $.tfim;
					}(modelEdoParam);
					if (A2($author$project$EdoSolver$checkEndTimeEps, tempo, tfimAnimation)) {
						return _Utils_Tuple2(
							$author$project$Main$SolvingEdo(modelAnimation),
							$elm$core$Platform$Cmd$none);
					} else {
						var tfimStep = function ($) {
							return $.tfim;
						}(animatingEdoParam);
						var refModel = function ($) {
							return $.refModel;
						}(modelAnimation);
						var refFunc = $author$project$Reference$refFunctionFromModel(refModel);
						var modelSystemModel = function ($) {
							return $.modelSystemModel;
						}(modelAnimation);
						var stateUpdatedModel = A2($author$project$ModelSystem$updateModelFromXs, xs, modelSystemModel);
						var modelData = function ($) {
							return $.chartData;
						}(modelAnimation);
						var dTimeSec = dTime / 1000.0;
						var newTfimStep = A2($elm$core$Basics$min, tfimAnimation, tfimStep + dTimeSec);
						var controlModel = function ($) {
							return $.controlModel;
						}(modelAnimation);
						var controller = $author$project$Controller$controllerFromModel(controlModel);
						var animatingEdoParam2 = _Utils_update(
							animatingEdoParam,
							{tfim: newTfimStep});
						var _v2 = A4($author$project$ModelSystem$runEdo, stateUpdatedModel, animatingEdoParam2, refFunc, controller);
						var data = _v2.a;
						var newAnimationEdoParam = _v2.b;
						var newData = _Utils_ap(data, modelData);
						var newModel = _Utils_update(
							modelAnimation,
							{chartData: newData});
						var newXs = function () {
							if (data.b) {
								var d = data.a;
								var ds = data.b;
								return $author$project$DataConvert$xsFromDatum(d);
							} else {
								return _List_Nil;
							}
						}();
						return _Utils_Tuple2(
							A3($author$project$Main$Animation, newModel, newAnimationEdoParam, newXs),
							$elm$core$Platform$Cmd$none);
					}
				}
		}
	});
var $author$project$Main$ControlMsg = function (a) {
	return {$: 'ControlMsg', a: a};
};
var $author$project$Main$EdoMsg = function (a) {
	return {$: 'EdoMsg', a: a};
};
var $author$project$Main$ModelSystemMsg = function (a) {
	return {$: 'ModelSystemMsg', a: a};
};
var $author$project$Main$RefMsg = function (a) {
	return {$: 'RefMsg', a: a};
};
var $author$project$Main$RunAnimation = {$: 'RunAnimation'};
var $author$project$Main$RunEdo = {$: 'RunEdo'};
var $mdgriffith$elm_ui$Internal$Model$AlignX = function (a) {
	return {$: 'AlignX', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Left = {$: 'Left'};
var $mdgriffith$elm_ui$Element$alignLeft = $mdgriffith$elm_ui$Internal$Model$AlignX($mdgriffith$elm_ui$Internal$Model$Left);
var $mdgriffith$elm_ui$Internal$Model$Right = {$: 'Right'};
var $mdgriffith$elm_ui$Element$alignRight = $mdgriffith$elm_ui$Internal$Model$AlignX($mdgriffith$elm_ui$Internal$Model$Right);
var $mdgriffith$elm_ui$Internal$Model$AlignY = function (a) {
	return {$: 'AlignY', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Top = {$: 'Top'};
var $mdgriffith$elm_ui$Element$alignTop = $mdgriffith$elm_ui$Internal$Model$AlignY($mdgriffith$elm_ui$Internal$Model$Top);
var $mdgriffith$elm_ui$Internal$Model$Attr = function (a) {
	return {$: 'Attr', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Button = {$: 'Button'};
var $mdgriffith$elm_ui$Internal$Model$Describe = function (a) {
	return {$: 'Describe', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Unkeyed = function (a) {
	return {$: 'Unkeyed', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$AsEl = {$: 'AsEl'};
var $mdgriffith$elm_ui$Internal$Model$asEl = $mdgriffith$elm_ui$Internal$Model$AsEl;
var $mdgriffith$elm_ui$Internal$Style$classes = {above: 'a', active: 'atv', alignBottom: 'ab', alignCenterX: 'cx', alignCenterY: 'cy', alignContainerBottom: 'acb', alignContainerCenterX: 'accx', alignContainerCenterY: 'accy', alignContainerRight: 'acr', alignLeft: 'al', alignRight: 'ar', alignTop: 'at', alignedHorizontally: 'ah', alignedVertically: 'av', any: 's', behind: 'bh', below: 'b', bold: 'w7', borderDashed: 'bd', borderDotted: 'bdt', borderNone: 'bn', borderSolid: 'bs', capturePointerEvents: 'cpe', clip: 'cp', clipX: 'cpx', clipY: 'cpy', column: 'c', container: 'ctr', contentBottom: 'cb', contentCenterX: 'ccx', contentCenterY: 'ccy', contentLeft: 'cl', contentRight: 'cr', contentTop: 'ct', cursorPointer: 'cptr', cursorText: 'ctxt', focus: 'fcs', focusedWithin: 'focus-within', fullSize: 'fs', grid: 'g', hasBehind: 'hbh', heightContent: 'hc', heightExact: 'he', heightFill: 'hf', heightFillPortion: 'hfp', hover: 'hv', imageContainer: 'ic', inFront: 'fr', inputLabel: 'lbl', inputMultiline: 'iml', inputMultilineFiller: 'imlf', inputMultilineParent: 'imlp', inputMultilineWrapper: 'implw', inputText: 'it', italic: 'i', link: 'lnk', nearby: 'nb', noTextSelection: 'notxt', onLeft: 'ol', onRight: 'or', opaque: 'oq', overflowHidden: 'oh', page: 'pg', paragraph: 'p', passPointerEvents: 'ppe', root: 'ui', row: 'r', scrollbars: 'sb', scrollbarsX: 'sbx', scrollbarsY: 'sby', seButton: 'sbt', single: 'e', sizeByCapital: 'cap', spaceEvenly: 'sev', strike: 'sk', text: 't', textCenter: 'tc', textExtraBold: 'w8', textExtraLight: 'w2', textHeavy: 'w9', textJustify: 'tj', textJustifyAll: 'tja', textLeft: 'tl', textLight: 'w3', textMedium: 'w5', textNormalWeight: 'w4', textRight: 'tr', textSemiBold: 'w6', textThin: 'w1', textUnitalicized: 'tun', transition: 'ts', transparent: 'clr', underline: 'u', widthContent: 'wc', widthExact: 'we', widthFill: 'wf', widthFillPortion: 'wfp', wrapped: 'wrp'};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $mdgriffith$elm_ui$Internal$Model$Generic = {$: 'Generic'};
var $mdgriffith$elm_ui$Internal$Model$div = $mdgriffith$elm_ui$Internal$Model$Generic;
var $mdgriffith$elm_ui$Internal$Model$NoNearbyChildren = {$: 'NoNearbyChildren'};
var $mdgriffith$elm_ui$Internal$Model$columnClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.column);
var $mdgriffith$elm_ui$Internal$Model$gridClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.grid);
var $mdgriffith$elm_ui$Internal$Model$pageClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.page);
var $mdgriffith$elm_ui$Internal$Model$paragraphClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.paragraph);
var $mdgriffith$elm_ui$Internal$Model$rowClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.row);
var $mdgriffith$elm_ui$Internal$Model$singleClass = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.single);
var $mdgriffith$elm_ui$Internal$Model$contextClasses = function (context) {
	switch (context.$) {
		case 'AsRow':
			return $mdgriffith$elm_ui$Internal$Model$rowClass;
		case 'AsColumn':
			return $mdgriffith$elm_ui$Internal$Model$columnClass;
		case 'AsEl':
			return $mdgriffith$elm_ui$Internal$Model$singleClass;
		case 'AsGrid':
			return $mdgriffith$elm_ui$Internal$Model$gridClass;
		case 'AsParagraph':
			return $mdgriffith$elm_ui$Internal$Model$paragraphClass;
		default:
			return $mdgriffith$elm_ui$Internal$Model$pageClass;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Keyed = function (a) {
	return {$: 'Keyed', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$NoStyleSheet = {$: 'NoStyleSheet'};
var $mdgriffith$elm_ui$Internal$Model$Styled = function (a) {
	return {$: 'Styled', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$Unstyled = function (a) {
	return {$: 'Unstyled', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addChildren = F2(
	function (existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 'NoNearbyChildren':
				return existing;
			case 'ChildrenBehind':
				var behind = nearbyChildren.a;
				return _Utils_ap(behind, existing);
			case 'ChildrenInFront':
				var inFront = nearbyChildren.a;
				return _Utils_ap(existing, inFront);
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					behind,
					_Utils_ap(existing, inFront));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$addKeyedChildren = F3(
	function (key, existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 'NoNearbyChildren':
				return existing;
			case 'ChildrenBehind':
				var behind = nearbyChildren.a;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					existing);
			case 'ChildrenInFront':
				var inFront = nearbyChildren.a;
				return _Utils_ap(
					existing,
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						inFront));
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					_Utils_ap(
						existing,
						A2(
							$elm$core$List$map,
							function (x) {
								return _Utils_Tuple2(key, x);
							},
							inFront)));
		}
	});
var $mdgriffith$elm_ui$Internal$Model$AsParagraph = {$: 'AsParagraph'};
var $mdgriffith$elm_ui$Internal$Model$asParagraph = $mdgriffith$elm_ui$Internal$Model$AsParagraph;
var $mdgriffith$elm_ui$Internal$Flag$Flag = function (a) {
	return {$: 'Flag', a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Second = function (a) {
	return {$: 'Second', a: a};
};
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $mdgriffith$elm_ui$Internal$Flag$flag = function (i) {
	return (i > 31) ? $mdgriffith$elm_ui$Internal$Flag$Second(1 << (i - 32)) : $mdgriffith$elm_ui$Internal$Flag$Flag(1 << i);
};
var $mdgriffith$elm_ui$Internal$Flag$alignBottom = $mdgriffith$elm_ui$Internal$Flag$flag(41);
var $mdgriffith$elm_ui$Internal$Flag$alignRight = $mdgriffith$elm_ui$Internal$Flag$flag(40);
var $mdgriffith$elm_ui$Internal$Flag$centerX = $mdgriffith$elm_ui$Internal$Flag$flag(42);
var $mdgriffith$elm_ui$Internal$Flag$centerY = $mdgriffith$elm_ui$Internal$Flag$flag(43);
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $mdgriffith$elm_ui$Internal$Model$lengthClassName = function (x) {
	switch (x.$) {
		case 'Px':
			var px = x.a;
			return $elm$core$String$fromInt(px) + 'px';
		case 'Content':
			return 'auto';
		case 'Fill':
			var i = x.a;
			return $elm$core$String$fromInt(i) + 'fr';
		case 'Min':
			var min = x.a;
			var len = x.b;
			return 'min' + ($elm$core$String$fromInt(min) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
		default:
			var max = x.a;
			var len = x.b;
			return 'max' + ($elm$core$String$fromInt(max) + $mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
	}
};
var $elm$core$Basics$round = _Basics_round;
var $mdgriffith$elm_ui$Internal$Model$floatClass = function (x) {
	return $elm$core$String$fromInt(
		$elm$core$Basics$round(x * 255));
};
var $mdgriffith$elm_ui$Internal$Model$transformClass = function (transform) {
	switch (transform.$) {
		case 'Untransformed':
			return $elm$core$Maybe$Nothing;
		case 'Moved':
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'mv-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(x) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(y) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(z))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			return $elm$core$Maybe$Just(
				'tfrm-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ty) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(tz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sx) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(sz) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(ox) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oy) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(oz) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(angle))))))))))))))))))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$getStyleName = function (style) {
	switch (style.$) {
		case 'Shadows':
			var name = style.a;
			return name;
		case 'Transparency':
			var name = style.a;
			var o = style.b;
			return name;
		case 'Style':
			var _class = style.a;
			return _class;
		case 'FontFamily':
			var name = style.a;
			return name;
		case 'FontSize':
			var i = style.a;
			return 'font-size-' + $elm$core$String$fromInt(i);
		case 'Single':
			var _class = style.a;
			return _class;
		case 'Colored':
			var _class = style.a;
			return _class;
		case 'SpacingStyle':
			var cls = style.a;
			var x = style.b;
			var y = style.c;
			return cls;
		case 'PaddingStyle':
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 'BorderWidth':
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 'GridTemplateStyle':
			var template = style.a;
			return 'grid-rows-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.rows)) + ('-cols-' + (A2(
				$elm$core$String$join,
				'-',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.columns)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.b)))))));
		case 'GridPosition':
			var pos = style.a;
			return 'gp grid-pos-' + ($elm$core$String$fromInt(pos.row) + ('-' + ($elm$core$String$fromInt(pos.col) + ('-' + ($elm$core$String$fromInt(pos.width) + ('-' + $elm$core$String$fromInt(pos.height)))))));
		case 'PseudoSelector':
			var selector = style.a;
			var subStyle = style.b;
			var name = function () {
				switch (selector.$) {
					case 'Focus':
						return 'fs';
					case 'Hover':
						return 'hv';
					default:
						return 'act';
				}
			}();
			return A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$map,
					function (sty) {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$getStyleName(sty);
						if (_v1 === '') {
							return '';
						} else {
							var styleName = _v1;
							return styleName + ('-' + name);
						}
					},
					subStyle));
		default:
			var x = style.a;
			return A2(
				$elm$core$Maybe$withDefault,
				'',
				$mdgriffith$elm_ui$Internal$Model$transformClass(x));
	}
};
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $mdgriffith$elm_ui$Internal$Model$reduceStyles = F2(
	function (style, nevermind) {
		var cache = nevermind.a;
		var existing = nevermind.b;
		var styleName = $mdgriffith$elm_ui$Internal$Model$getStyleName(style);
		return A2($elm$core$Set$member, styleName, cache) ? nevermind : _Utils_Tuple2(
			A2($elm$core$Set$insert, styleName, cache),
			A2($elm$core$List$cons, style, existing));
	});
var $mdgriffith$elm_ui$Internal$Model$Property = F2(
	function (a, b) {
		return {$: 'Property', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$Style = F2(
	function (a, b) {
		return {$: 'Style', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$dot = function (c) {
	return '.' + c;
};
var $mdgriffith$elm_ui$Internal$Model$formatColor = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return 'rgba(' + ($elm$core$String$fromInt(
		$elm$core$Basics$round(red * 255)) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(green * 255))) + ((',' + $elm$core$String$fromInt(
		$elm$core$Basics$round(blue * 255))) + (',' + ($elm$core$String$fromFloat(alpha) + ')')))));
};
var $mdgriffith$elm_ui$Internal$Model$formatBoxShadow = function (shadow) {
	return A2(
		$elm$core$String$join,
		' ',
		A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			_List_fromArray(
				[
					shadow.inset ? $elm$core$Maybe$Just('inset') : $elm$core$Maybe$Nothing,
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.offset.a) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.offset.b) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.blur) + 'px'),
					$elm$core$Maybe$Just(
					$elm$core$String$fromFloat(shadow.size) + 'px'),
					$elm$core$Maybe$Just(
					$mdgriffith$elm_ui$Internal$Model$formatColor(shadow.color))
				])));
};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $mdgriffith$elm_ui$Internal$Model$renderFocusStyle = function (focus) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.focusedWithin) + ':focus-within',
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.borderColor),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.backgroundColor),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										blur: shadow.blur,
										color: shadow.color,
										inset: false,
										offset: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.offset)),
										size: shadow.size
									}));
						},
						focus.shadow),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					]))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$Style,
			($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ':focus .focusable, ') + (($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + '.focusable:focus, ') + ('.ui-slide-bar:focus + ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ' .focusable-thumb'))),
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'border-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.borderColor),
						A2(
						$elm$core$Maybe$map,
						function (color) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'background-color',
								$mdgriffith$elm_ui$Internal$Model$formatColor(color));
						},
						focus.backgroundColor),
						A2(
						$elm$core$Maybe$map,
						function (shadow) {
							return A2(
								$mdgriffith$elm_ui$Internal$Model$Property,
								'box-shadow',
								$mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
									{
										blur: shadow.blur,
										color: shadow.color,
										inset: false,
										offset: A2(
											$elm$core$Tuple$mapSecond,
											$elm$core$Basics$toFloat,
											A2($elm$core$Tuple$mapFirst, $elm$core$Basics$toFloat, shadow.offset)),
										size: shadow.size
									}));
						},
						focus.shadow),
						$elm$core$Maybe$Just(
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
					])))
		]);
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Style$AllChildren = F2(
	function (a, b) {
		return {$: 'AllChildren', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Batch = function (a) {
	return {$: 'Batch', a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Child = F2(
	function (a, b) {
		return {$: 'Child', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Class = F2(
	function (a, b) {
		return {$: 'Class', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Descriptor = F2(
	function (a, b) {
		return {$: 'Descriptor', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Left = {$: 'Left'};
var $mdgriffith$elm_ui$Internal$Style$Prop = F2(
	function (a, b) {
		return {$: 'Prop', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Right = {$: 'Right'};
var $mdgriffith$elm_ui$Internal$Style$Self = function (a) {
	return {$: 'Self', a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Supports = F2(
	function (a, b) {
		return {$: 'Supports', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Style$Content = function (a) {
	return {$: 'Content', a: a};
};
var $mdgriffith$elm_ui$Internal$Style$Bottom = {$: 'Bottom'};
var $mdgriffith$elm_ui$Internal$Style$CenterX = {$: 'CenterX'};
var $mdgriffith$elm_ui$Internal$Style$CenterY = {$: 'CenterY'};
var $mdgriffith$elm_ui$Internal$Style$Top = {$: 'Top'};
var $mdgriffith$elm_ui$Internal$Style$alignments = _List_fromArray(
	[$mdgriffith$elm_ui$Internal$Style$Top, $mdgriffith$elm_ui$Internal$Style$Bottom, $mdgriffith$elm_ui$Internal$Style$Right, $mdgriffith$elm_ui$Internal$Style$Left, $mdgriffith$elm_ui$Internal$Style$CenterX, $mdgriffith$elm_ui$Internal$Style$CenterY]);
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $mdgriffith$elm_ui$Internal$Style$contentName = function (desc) {
	switch (desc.a.$) {
		case 'Top':
			var _v1 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentTop);
		case 'Bottom':
			var _v2 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentBottom);
		case 'Right':
			var _v3 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentRight);
		case 'Left':
			var _v4 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentLeft);
		case 'CenterX':
			var _v5 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentCenterX);
		default:
			var _v6 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.contentCenterY);
	}
};
var $mdgriffith$elm_ui$Internal$Style$selfName = function (desc) {
	switch (desc.a.$) {
		case 'Top':
			var _v1 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignTop);
		case 'Bottom':
			var _v2 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignBottom);
		case 'Right':
			var _v3 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignRight);
		case 'Left':
			var _v4 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignLeft);
		case 'CenterX':
			var _v5 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterX);
		default:
			var _v6 = desc.a;
			return $mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterY);
	}
};
var $mdgriffith$elm_ui$Internal$Style$describeAlignment = function (values) {
	var createDescription = function (alignment) {
		var _v0 = values(alignment);
		var content = _v0.a;
		var indiv = _v0.b;
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$contentName(
					$mdgriffith$elm_ui$Internal$Style$Content(alignment)),
				content),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(
							$mdgriffith$elm_ui$Internal$Style$Self(alignment)),
						indiv)
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$elDescription = _List_fromArray(
	[
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
		A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.hasBehind),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.behind),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Descriptor,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.seButton),
		_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.text),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'auto !important')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightContent),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Child,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthContent),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
			])),
		$mdgriffith$elm_ui$Internal$Style$describeAlignment(
		function (alignment) {
			switch (alignment.$) {
				case 'Top':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
							]));
				case 'Bottom':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
							]));
				case 'Right':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
							]));
				case 'Left':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							]));
				case 'CenterX':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
							]));
				default:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
									]))
							]),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
							]));
			}
		})
	]);
var $mdgriffith$elm_ui$Internal$Style$gridAlignments = function (values) {
	var createDescription = function (alignment) {
		return _List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$selfName(
							$mdgriffith$elm_ui$Internal$Style$Self(alignment)),
						values(alignment))
					]))
			]);
	};
	return $mdgriffith$elm_ui$Internal$Style$Batch(
		A2($elm$core$List$concatMap, createDescription, $mdgriffith$elm_ui$Internal$Style$alignments));
};
var $mdgriffith$elm_ui$Internal$Style$Above = {$: 'Above'};
var $mdgriffith$elm_ui$Internal$Style$Behind = {$: 'Behind'};
var $mdgriffith$elm_ui$Internal$Style$Below = {$: 'Below'};
var $mdgriffith$elm_ui$Internal$Style$OnLeft = {$: 'OnLeft'};
var $mdgriffith$elm_ui$Internal$Style$OnRight = {$: 'OnRight'};
var $mdgriffith$elm_ui$Internal$Style$Within = {$: 'Within'};
var $mdgriffith$elm_ui$Internal$Style$locations = function () {
	var loc = $mdgriffith$elm_ui$Internal$Style$Above;
	var _v0 = function () {
		switch (loc.$) {
			case 'Above':
				return _Utils_Tuple0;
			case 'Below':
				return _Utils_Tuple0;
			case 'OnRight':
				return _Utils_Tuple0;
			case 'OnLeft':
				return _Utils_Tuple0;
			case 'Within':
				return _Utils_Tuple0;
			default:
				return _Utils_Tuple0;
		}
	}();
	return _List_fromArray(
		[$mdgriffith$elm_ui$Internal$Style$Above, $mdgriffith$elm_ui$Internal$Style$Below, $mdgriffith$elm_ui$Internal$Style$OnRight, $mdgriffith$elm_ui$Internal$Style$OnLeft, $mdgriffith$elm_ui$Internal$Style$Within, $mdgriffith$elm_ui$Internal$Style$Behind]);
}();
var $mdgriffith$elm_ui$Internal$Style$baseSheet = _List_fromArray(
	[
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		'html,body',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		_Utils_ap(
			$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
			_Utils_ap(
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.imageContainer))),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'img',
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'max-height', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'object-fit', 'cover')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'img',
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'max-width', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'object-fit', 'cover')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ':focus',
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'outline', 'none')
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.root),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', '100%'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Child,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inFront),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.nearby),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20')
							]))
					]))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.nearby),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				$mdgriffith$elm_ui$Internal$Style$Batch(
				function (fn) {
					return A2($elm$core$List$map, fn, $mdgriffith$elm_ui$Internal$Style$locations);
				}(
					function (loc) {
						switch (loc.$) {
							case 'Above':
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.above),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
												])),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 'Below':
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.below),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												])),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												]))
										]));
							case 'OnRight':
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.onRight),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 'OnLeft':
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.onLeft),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'right', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 'Within':
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inFront),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							default:
								return A2(
									$mdgriffith$elm_ui$Internal$Style$Descriptor,
									$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.behind),
									_List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											$mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
						}
					}))
			])),
		A2(
		$mdgriffith$elm_ui$Internal$Style$Class,
		$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
		_List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'resize', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'box-sizing', 'border-box'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-size', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-family', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', 'inherit'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'none'),
				A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'inherit'),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.wrapped),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-wrap', 'wrap')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.noTextSelection),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-moz-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-webkit-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, '-ms-user-select', 'none'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'user-select', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cursorPointer),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'pointer')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.cursorText),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.passPointerEvents),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.capturePointerEvents),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto !important')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.transparent),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.opaque),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.hover, $mdgriffith$elm_ui$Internal$Style$classes.transparent)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.hover, $mdgriffith$elm_ui$Internal$Style$classes.opaque)) + ':hover',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.focus, $mdgriffith$elm_ui$Internal$Style$classes.transparent)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.focus, $mdgriffith$elm_ui$Internal$Style$classes.opaque)) + ':focus',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.active, $mdgriffith$elm_ui$Internal$Style$classes.transparent)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap($mdgriffith$elm_ui$Internal$Style$classes.active, $mdgriffith$elm_ui$Internal$Style$classes.opaque)) + ':active',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.transition),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Prop,
						'transition',
						A2(
							$elm$core$String$join,
							', ',
							A2(
								$elm$core$List$map,
								function (x) {
									return x + ' 160ms';
								},
								_List_fromArray(
									['transform', 'opacity', 'filter', 'background-color', 'color', 'font-size']))))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.scrollbars),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.scrollbarsX),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.row),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.scrollbarsY),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'auto'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.column),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.clip),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.clipX),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.clipY),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'hidden')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthContent),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', 'auto')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.borderNone),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.borderDashed),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dashed')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.borderDotted),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dotted')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.borderSolid),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.text),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputText),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1.05'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background', 'transparent'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'inherit')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
				$mdgriffith$elm_ui$Internal$Style$elDescription),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.row),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthExact),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.link),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFillPortion),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.container),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerRight,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterX),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-left', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterX),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-right', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.alignContainerRight + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 'Bottom':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 'Right':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_Nil);
								case 'Left':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_Nil);
								case 'CenterX':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.spaceEvenly),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputLabel),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'baseline')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.column),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0px'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', 'min-content'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightExact),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFill),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthContent),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerBottom,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY + ' ~ u'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + ($mdgriffith$elm_ui$Internal$Style$classes.alignContainerBottom + (' ~ s.' + $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY)),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
											]));
								case 'Bottom':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto')
											]));
								case 'Right':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 'Left':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 'CenterX':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
							}
						}),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.container),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.spaceEvenly),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.grid),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', '-ms-grid'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						'.gp',
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Supports,
						_Utils_Tuple2('display', 'grid'),
						_List_fromArray(
							[
								_Utils_Tuple2('display', 'grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$gridAlignments(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
										]);
								case 'Bottom':
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
										]);
								case 'Right':
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
										]);
								case 'Left':
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
										]);
								case 'CenterX':
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
										]);
								default:
									return _List_fromArray(
										[
											A2($mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
										]);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.page),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any + ':first-child'),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.any + ($mdgriffith$elm_ui$Internal$Style$selfName(
								$mdgriffith$elm_ui$Internal$Style$Self($mdgriffith$elm_ui$Internal$Style$Left)) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.any))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot(
							$mdgriffith$elm_ui$Internal$Style$classes.any + ($mdgriffith$elm_ui$Internal$Style$selfName(
								$mdgriffith$elm_ui$Internal$Style$Self($mdgriffith$elm_ui$Internal$Style$Right)) + (':first-child + .' + $mdgriffith$elm_ui$Internal$Style$classes.any))),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Bottom':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Right':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 'Left':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left'),
												A2(
												$mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2($mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 'CenterX':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputMultiline),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'background-color', 'transparent')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineWrapper),
				_List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineParent),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineFiller),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap !important'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'transparent')
							]))
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.paragraph),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-wrap', 'break-word'),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Descriptor,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.hasBehind),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.behind),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.text),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.paragraph),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								'::after',
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', 'none')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								'::before',
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'content', 'none')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$AllChildren,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.single),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.widthExact),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.inFront),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.behind),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.above),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.below),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.onRight),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Descriptor,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.onLeft),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								$mdgriffith$elm_ui$Internal$Style$Child,
								$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.text),
								_List_fromArray(
									[
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
										A2($mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
									]))
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.row),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.column),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						$mdgriffith$elm_ui$Internal$Style$Child,
						$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.grid),
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-grid')
							])),
						$mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Bottom':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Right':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right')
											]));
								case 'Left':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2($mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left')
											]));
								case 'CenterX':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.hidden',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'none')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textThin),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '100')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textExtraLight),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '200')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textLight),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '300')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textNormalWeight),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '400')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textMedium),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '500')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textSemiBold),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '600')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.bold),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '700')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textExtraBold),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '800')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textHeavy),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '900')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.italic),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'italic')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.strike),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.underline),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.underline),
					$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.strike)),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through underline'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textUnitalicized),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'normal')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textJustify),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textJustifyAll),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify-all')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textCenter),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'center')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textRight),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'right')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				$mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.textLeft),
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'left')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.modal',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none')
					]))
			]))
	]);
var $mdgriffith$elm_ui$Internal$Style$fontVariant = function (_var) {
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + _var,
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\"'))
				])),
			A2(
			$mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + (_var + '-off'),
			_List_fromArray(
				[
					A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\" 0'))
				]))
		]);
};
var $mdgriffith$elm_ui$Internal$Style$commonValues = $elm$core$List$concat(
	_List_fromArray(
		[
			A2(
			$elm$core$List$map,
			function (x) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.border-' + $elm$core$String$fromInt(x),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'border-width',
							$elm$core$String$fromInt(x) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 6)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 8, 32)),
			A2(
			$elm$core$List$map,
			function (i) {
				return A2(
					$mdgriffith$elm_ui$Internal$Style$Class,
					'.p-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Style$Prop,
							'padding',
							$elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2($elm$core$List$range, 0, 24)),
			_List_fromArray(
			[
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'small-caps')
					])),
				A2(
				$mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp-off',
				_List_fromArray(
					[
						A2($mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'normal')
					]))
			]),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('zero'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('onum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('liga'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('dlig'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('ordn'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('tnum'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('afrc'),
			$mdgriffith$elm_ui$Internal$Style$fontVariant('frac')
		]));
var $mdgriffith$elm_ui$Internal$Style$explainer = '\n.explain {\n    border: 6px solid rgb(174, 121, 15) !important;\n}\n.explain > .' + ($mdgriffith$elm_ui$Internal$Style$classes.any + (' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n.ctr {\n    border: none !important;\n}\n.explain > .ctr > .' + ($mdgriffith$elm_ui$Internal$Style$classes.any + ' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n')));
var $mdgriffith$elm_ui$Internal$Style$inputTextReset = '\ninput[type="search"],\ninput[type="search"]::-webkit-search-decoration,\ninput[type="search"]::-webkit-search-cancel-button,\ninput[type="search"]::-webkit-search-results-button,\ninput[type="search"]::-webkit-search-results-decoration {\n  -webkit-appearance:none;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$sliderReset = '\ninput[type=range] {\n  -webkit-appearance: none; \n  background: transparent;\n  position:absolute;\n  left:0;\n  top:0;\n  z-index:10;\n  width: 100%;\n  outline: dashed 1px;\n  height: 100%;\n  opacity: 0;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$thumbReset = '\ninput[type=range]::-webkit-slider-thumb {\n    -webkit-appearance: none;\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-moz-range-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-ms-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range][orient=vertical]{\n    writing-mode: bt-lr; /* IE */\n    -webkit-appearance: slider-vertical;  /* WebKit */\n}\n';
var $mdgriffith$elm_ui$Internal$Style$trackReset = '\ninput[type=range]::-moz-range-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-ms-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-webkit-slider-runnable-track {\n    background: transparent;\n    cursor: pointer;\n}\n';
var $mdgriffith$elm_ui$Internal$Style$overrides = '@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.row) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + (' { flex-basis: auto !important; } ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.row) + (' > ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.container) + (' { flex-basis: auto !important; }}' + ($mdgriffith$elm_ui$Internal$Style$inputTextReset + ($mdgriffith$elm_ui$Internal$Style$sliderReset + ($mdgriffith$elm_ui$Internal$Style$trackReset + ($mdgriffith$elm_ui$Internal$Style$thumbReset + $mdgriffith$elm_ui$Internal$Style$explainer)))))))))))))));
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $mdgriffith$elm_ui$Internal$Style$Intermediate = function (a) {
	return {$: 'Intermediate', a: a};
};
var $mdgriffith$elm_ui$Internal$Style$emptyIntermediate = F2(
	function (selector, closing) {
		return $mdgriffith$elm_ui$Internal$Style$Intermediate(
			{closing: closing, others: _List_Nil, props: _List_Nil, selector: selector});
	});
var $mdgriffith$elm_ui$Internal$Style$renderRules = F2(
	function (_v0, rulesToRender) {
		var parent = _v0.a;
		var generateIntermediates = F2(
			function (rule, rendered) {
				switch (rule.$) {
					case 'Prop':
						var name = rule.a;
						var val = rule.b;
						return _Utils_update(
							rendered,
							{
								props: A2(
									$elm$core$List$cons,
									_Utils_Tuple2(name, val),
									rendered.props)
							});
					case 'Supports':
						var _v2 = rule.a;
						var prop = _v2.a;
						var value = _v2.b;
						var props = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Internal$Style$Intermediate(
										{closing: '\n}', others: _List_Nil, props: props, selector: '@supports (' + (prop + (':' + (value + (') {' + parent.selector))))}),
									rendered.others)
							});
					case 'Adjacent':
						var selector = rule.a;
						var adjRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector + (' + ' + selector), ''),
										adjRules),
									rendered.others)
							});
					case 'Child':
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector + (' > ' + child), ''),
										childRules),
									rendered.others)
							});
					case 'AllChildren':
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector + (' ' + child), ''),
										childRules),
									rendered.others)
							});
					case 'Descriptor':
						var descriptor = rule.a;
						var descriptorRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(
											$mdgriffith$elm_ui$Internal$Style$emptyIntermediate,
											_Utils_ap(parent.selector, descriptor),
											''),
										descriptorRules),
									rendered.others)
							});
					default:
						var batched = rule.a;
						return _Utils_update(
							rendered,
							{
								others: A2(
									$elm$core$List$cons,
									A2(
										$mdgriffith$elm_ui$Internal$Style$renderRules,
										A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector, ''),
										batched),
									rendered.others)
							});
				}
			});
		return $mdgriffith$elm_ui$Internal$Style$Intermediate(
			A3($elm$core$List$foldr, generateIntermediates, parent, rulesToRender));
	});
var $mdgriffith$elm_ui$Internal$Style$renderCompact = function (styleClasses) {
	var renderValues = function (values) {
		return $elm$core$String$concat(
			A2(
				$elm$core$List$map,
				function (_v3) {
					var x = _v3.a;
					var y = _v3.b;
					return x + (':' + (y + ';'));
				},
				values));
	};
	var renderClass = function (rule) {
		var _v2 = rule.props;
		if (!_v2.b) {
			return '';
		} else {
			return rule.selector + ('{' + (renderValues(rule.props) + (rule.closing + '}')));
		}
	};
	var renderIntermediate = function (_v0) {
		var rule = _v0.a;
		return _Utils_ap(
			renderClass(rule),
			$elm$core$String$concat(
				A2($elm$core$List$map, renderIntermediate, rule.others)));
	};
	return $elm$core$String$concat(
		A2(
			$elm$core$List$map,
			renderIntermediate,
			A3(
				$elm$core$List$foldr,
				F2(
					function (_v1, existing) {
						var name = _v1.a;
						var styleRules = _v1.b;
						return A2(
							$elm$core$List$cons,
							A2(
								$mdgriffith$elm_ui$Internal$Style$renderRules,
								A2($mdgriffith$elm_ui$Internal$Style$emptyIntermediate, name, ''),
								styleRules),
							existing);
					}),
				_List_Nil,
				styleClasses)));
};
var $mdgriffith$elm_ui$Internal$Style$rules = _Utils_ap(
	$mdgriffith$elm_ui$Internal$Style$overrides,
	$mdgriffith$elm_ui$Internal$Style$renderCompact(
		_Utils_ap($mdgriffith$elm_ui$Internal$Style$baseSheet, $mdgriffith$elm_ui$Internal$Style$commonValues)));
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $mdgriffith$elm_ui$Internal$Model$staticRoot = function (opts) {
	var _v0 = opts.mode;
	switch (_v0.$) {
		case 'Layout':
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'div',
				_List_Nil,
				_List_fromArray(
					[
						A3(
						$elm$virtual_dom$VirtualDom$node,
						'style',
						_List_Nil,
						_List_fromArray(
							[
								$elm$virtual_dom$VirtualDom$text($mdgriffith$elm_ui$Internal$Style$rules)
							]))
					]));
		case 'NoStaticStyleSheet':
			return $elm$virtual_dom$VirtualDom$text('');
		default:
			return A3(
				$elm$virtual_dom$VirtualDom$node,
				'elm-ui-static-rules',
				_List_fromArray(
					[
						A2(
						$elm$virtual_dom$VirtualDom$property,
						'rules',
						$elm$json$Json$Encode$string($mdgriffith$elm_ui$Internal$Style$rules))
					]),
				_List_Nil);
	}
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$fontName = function (font) {
	switch (font.$) {
		case 'Serif':
			return 'serif';
		case 'SansSerif':
			return 'sans-serif';
		case 'Monospace':
			return 'monospace';
		case 'Typeface':
			var name = font.a;
			return '\"' + (name + '\"');
		case 'ImportFont':
			var name = font.a;
			var url = font.b;
			return '\"' + (name + '\"');
		default:
			var name = font.a.name;
			return '\"' + (name + '\"');
	}
};
var $mdgriffith$elm_ui$Internal$Model$isSmallCaps = function (_var) {
	switch (_var.$) {
		case 'VariantActive':
			var name = _var.a;
			return name === 'smcp';
		case 'VariantOff':
			var name = _var.a;
			return false;
		default:
			var name = _var.a;
			var index = _var.b;
			return (name === 'smcp') && (index === 1);
	}
};
var $mdgriffith$elm_ui$Internal$Model$hasSmallCaps = function (typeface) {
	if (typeface.$ === 'FontWith') {
		var font = typeface.a;
		return A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$isSmallCaps, font.variants);
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderProps = F3(
	function (force, _v0, existing) {
		var key = _v0.a;
		var val = _v0.b;
		return force ? (existing + ('\n  ' + (key + (': ' + (val + ' !important;'))))) : (existing + ('\n  ' + (key + (': ' + (val + ';')))));
	});
var $mdgriffith$elm_ui$Internal$Model$renderStyle = F4(
	function (options, maybePseudo, selector, props) {
		if (maybePseudo.$ === 'Nothing') {
			return _List_fromArray(
				[
					selector + ('{' + (A3(
					$elm$core$List$foldl,
					$mdgriffith$elm_ui$Internal$Model$renderProps(false),
					'',
					props) + '\n}'))
				]);
		} else {
			var pseudo = maybePseudo.a;
			switch (pseudo.$) {
				case 'Hover':
					var _v2 = options.hover;
					switch (_v2.$) {
						case 'NoHover':
							return _List_Nil;
						case 'ForceHover':
							return _List_fromArray(
								[
									selector + ('-hv {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(true),
									'',
									props) + '\n}'))
								]);
						default:
							return _List_fromArray(
								[
									selector + ('-hv:hover {' + (A3(
									$elm$core$List$foldl,
									$mdgriffith$elm_ui$Internal$Model$renderProps(false),
									'',
									props) + '\n}'))
								]);
					}
				case 'Focus':
					var renderedProps = A3(
						$elm$core$List$foldl,
						$mdgriffith$elm_ui$Internal$Model$renderProps(false),
						'',
						props);
					return _List_fromArray(
						[
							selector + ('-fs:focus {' + (renderedProps + '\n}')),
							('.' + ($mdgriffith$elm_ui$Internal$Style$classes.any + (':focus ' + (selector + '-fs  {')))) + (renderedProps + '\n}'),
							(selector + '-fs:focus-within {') + (renderedProps + '\n}'),
							('.ui-slide-bar:focus + ' + ($mdgriffith$elm_ui$Internal$Style$dot($mdgriffith$elm_ui$Internal$Style$classes.any) + (' .focusable-thumb' + (selector + '-fs {')))) + (renderedProps + '\n}')
						]);
				default:
					return _List_fromArray(
						[
							selector + ('-act:active {' + (A3(
							$elm$core$List$foldl,
							$mdgriffith$elm_ui$Internal$Model$renderProps(false),
							'',
							props) + '\n}'))
						]);
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderVariant = function (_var) {
	switch (_var.$) {
		case 'VariantActive':
			var name = _var.a;
			return '\"' + (name + '\"');
		case 'VariantOff':
			var name = _var.a;
			return '\"' + (name + '\" 0');
		default:
			var name = _var.a;
			var index = _var.b;
			return '\"' + (name + ('\" ' + $elm$core$String$fromInt(index)));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderVariants = function (typeface) {
	if (typeface.$ === 'FontWith') {
		var font = typeface.a;
		return $elm$core$Maybe$Just(
			A2(
				$elm$core$String$join,
				', ',
				A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$renderVariant, font.variants)));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$transformValue = function (transform) {
	switch (transform.$) {
		case 'Untransformed':
			return $elm$core$Maybe$Nothing;
		case 'Moved':
			var _v1 = transform.a;
			var x = _v1.a;
			var y = _v1.b;
			var z = _v1.c;
			return $elm$core$Maybe$Just(
				'translate3d(' + ($elm$core$String$fromFloat(x) + ('px, ' + ($elm$core$String$fromFloat(y) + ('px, ' + ($elm$core$String$fromFloat(z) + 'px)'))))));
		default:
			var _v2 = transform.a;
			var tx = _v2.a;
			var ty = _v2.b;
			var tz = _v2.c;
			var _v3 = transform.b;
			var sx = _v3.a;
			var sy = _v3.b;
			var sz = _v3.c;
			var _v4 = transform.c;
			var ox = _v4.a;
			var oy = _v4.b;
			var oz = _v4.c;
			var angle = transform.d;
			var translate = 'translate3d(' + ($elm$core$String$fromFloat(tx) + ('px, ' + ($elm$core$String$fromFloat(ty) + ('px, ' + ($elm$core$String$fromFloat(tz) + 'px)')))));
			var scale = 'scale3d(' + ($elm$core$String$fromFloat(sx) + (', ' + ($elm$core$String$fromFloat(sy) + (', ' + ($elm$core$String$fromFloat(sz) + ')')))));
			var rotate = 'rotate3d(' + ($elm$core$String$fromFloat(ox) + (', ' + ($elm$core$String$fromFloat(oy) + (', ' + ($elm$core$String$fromFloat(oz) + (', ' + ($elm$core$String$fromFloat(angle) + 'rad)')))))));
			return $elm$core$Maybe$Just(translate + (' ' + (scale + (' ' + rotate))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$renderStyleRule = F3(
	function (options, rule, maybePseudo) {
		switch (rule.$) {
			case 'Style':
				var selector = rule.a;
				var props = rule.b;
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, selector, props);
			case 'Shadows':
				var name = rule.a;
				var prop = rule.b;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, 'box-shadow', prop)
						]));
			case 'Transparency':
				var name = rule.a;
				var transparency = rule.b;
				var opacity = A2(
					$elm$core$Basics$max,
					0,
					A2($elm$core$Basics$min, 1, 1 - transparency));
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + name,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'opacity',
							$elm$core$String$fromFloat(opacity))
						]));
			case 'FontSize':
				var i = rule.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.font-size-' + $elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'font-size',
							$elm$core$String$fromInt(i) + 'px')
						]));
			case 'FontFamily':
				var name = rule.a;
				var typefaces = rule.b;
				var features = A2(
					$elm$core$String$join,
					', ',
					A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Internal$Model$renderVariants, typefaces));
				var families = _List_fromArray(
					[
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-family',
						A2(
							$elm$core$String$join,
							', ',
							A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$fontName, typefaces))),
						A2($mdgriffith$elm_ui$Internal$Model$Property, 'font-feature-settings', features),
						A2(
						$mdgriffith$elm_ui$Internal$Model$Property,
						'font-variant',
						A2($elm$core$List$any, $mdgriffith$elm_ui$Internal$Model$hasSmallCaps, typefaces) ? 'small-caps' : 'normal')
					]);
				return A4($mdgriffith$elm_ui$Internal$Model$renderStyle, options, maybePseudo, '.' + name, families);
			case 'Single':
				var _class = rule.a;
				var prop = rule.b;
				var val = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2($mdgriffith$elm_ui$Internal$Model$Property, prop, val)
						]));
			case 'Colored':
				var _class = rule.a;
				var prop = rule.b;
				var color = rule.c;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					'.' + _class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							prop,
							$mdgriffith$elm_ui$Internal$Model$formatColor(color))
						]));
			case 'SpacingStyle':
				var cls = rule.a;
				var x = rule.b;
				var y = rule.c;
				var yPx = $elm$core$String$fromInt(y) + 'px';
				var xPx = $elm$core$String$fromInt(x) + 'px';
				var single = '.' + $mdgriffith$elm_ui$Internal$Style$classes.single;
				var row = '.' + $mdgriffith$elm_ui$Internal$Style$classes.row;
				var wrappedRow = '.' + ($mdgriffith$elm_ui$Internal$Style$classes.wrapped + row);
				var right = '.' + $mdgriffith$elm_ui$Internal$Style$classes.alignRight;
				var paragraph = '.' + $mdgriffith$elm_ui$Internal$Style$classes.paragraph;
				var page = '.' + $mdgriffith$elm_ui$Internal$Style$classes.page;
				var left = '.' + $mdgriffith$elm_ui$Internal$Style$classes.alignLeft;
				var halfY = $elm$core$String$fromFloat(y / 2) + 'px';
				var halfX = $elm$core$String$fromFloat(x / 2) + 'px';
				var column = '.' + $mdgriffith$elm_ui$Internal$Style$classes.column;
				var _class = '.' + cls;
				var any = '.' + $mdgriffith$elm_ui$Internal$Style$classes.any;
				return $elm$core$List$concat(
					_List_fromArray(
						[
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (row + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (wrappedRow + (' > ' + any)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin', halfY + (' ' + halfX))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (column + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + (any + (' + ' + any)))),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (page + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_Utils_ap(_class, paragraph),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							'textarea' + (any + _class),
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'line-height',
									'calc(1em + ' + ($elm$core$String$fromInt(y) + 'px)')),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'height',
									'calc(100% + ' + ($elm$core$String$fromInt(y) + 'px)'))
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + left)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + (' > ' + right)),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::after'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-top',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								])),
							A4(
							$mdgriffith$elm_ui$Internal$Model$renderStyle,
							options,
							maybePseudo,
							_class + (paragraph + '::before'),
							_List_fromArray(
								[
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
									A2($mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
									A2(
									$mdgriffith$elm_ui$Internal$Model$Property,
									'margin-bottom',
									$elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
								]))
						]));
			case 'PaddingStyle':
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'padding',
							$elm$core$String$fromFloat(top) + ('px ' + ($elm$core$String$fromFloat(right) + ('px ' + ($elm$core$String$fromFloat(bottom) + ('px ' + ($elm$core$String$fromFloat(left) + 'px')))))))
						]));
			case 'BorderWidth':
				var cls = rule.a;
				var top = rule.b;
				var right = rule.c;
				var bottom = rule.d;
				var left = rule.e;
				var _class = '.' + cls;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$renderStyle,
					options,
					maybePseudo,
					_class,
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Internal$Model$Property,
							'border-width',
							$elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px')))))))
						]));
			case 'GridTemplateStyle':
				var template = rule.a;
				var toGridLengthHelper = F3(
					function (minimum, maximum, x) {
						toGridLengthHelper:
						while (true) {
							switch (x.$) {
								case 'Px':
									var px = x.a;
									return $elm$core$String$fromInt(px) + 'px';
								case 'Content':
									var _v2 = _Utils_Tuple2(minimum, maximum);
									if (_v2.a.$ === 'Nothing') {
										if (_v2.b.$ === 'Nothing') {
											var _v3 = _v2.a;
											var _v4 = _v2.b;
											return 'max-content';
										} else {
											var _v6 = _v2.a;
											var maxSize = _v2.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v2.b.$ === 'Nothing') {
											var minSize = _v2.a.a;
											var _v5 = _v2.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + 'max-content)'));
										} else {
											var minSize = _v2.a.a;
											var maxSize = _v2.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 'Fill':
									var i = x.a;
									var _v7 = _Utils_Tuple2(minimum, maximum);
									if (_v7.a.$ === 'Nothing') {
										if (_v7.b.$ === 'Nothing') {
											var _v8 = _v7.a;
											var _v9 = _v7.b;
											return $elm$core$String$fromInt(i) + 'fr';
										} else {
											var _v11 = _v7.a;
											var maxSize = _v7.b.a;
											return 'minmax(max-content, ' + ($elm$core$String$fromInt(maxSize) + 'px)');
										}
									} else {
										if (_v7.b.$ === 'Nothing') {
											var minSize = _v7.a.a;
											var _v10 = _v7.b;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(i) + ('fr' + 'fr)'))));
										} else {
											var minSize = _v7.a.a;
											var maxSize = _v7.b.a;
											return 'minmax(' + ($elm$core$String$fromInt(minSize) + ('px, ' + ($elm$core$String$fromInt(maxSize) + 'px)')));
										}
									}
								case 'Min':
									var m = x.a;
									var len = x.b;
									var $temp$minimum = $elm$core$Maybe$Just(m),
										$temp$maximum = maximum,
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
								default:
									var m = x.a;
									var len = x.b;
									var $temp$minimum = minimum,
										$temp$maximum = $elm$core$Maybe$Just(m),
										$temp$x = len;
									minimum = $temp$minimum;
									maximum = $temp$maximum;
									x = $temp$x;
									continue toGridLengthHelper;
							}
						}
					});
				var toGridLength = function (x) {
					return A3(toGridLengthHelper, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, x);
				};
				var xSpacing = toGridLength(template.spacing.a);
				var ySpacing = toGridLength(template.spacing.b);
				var rows = function (x) {
					return 'grid-template-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.rows)));
				var msRows = function (x) {
					return '-ms-grid-rows: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.columns)));
				var msColumns = function (x) {
					return '-ms-grid-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						ySpacing,
						A2($elm$core$List$map, toGridLength, template.columns)));
				var gapY = 'grid-row-gap:' + (toGridLength(template.spacing.b) + ';');
				var gapX = 'grid-column-gap:' + (toGridLength(template.spacing.a) + ';');
				var columns = function (x) {
					return 'grid-template-columns: ' + (x + ';');
				}(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, toGridLength, template.columns)));
				var _class = '.grid-rows-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.rows)) + ('-cols-' + (A2(
					$elm$core$String$join,
					'-',
					A2($elm$core$List$map, $mdgriffith$elm_ui$Internal$Model$lengthClassName, template.columns)) + ('-space-x-' + ($mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.a) + ('-space-y-' + $mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.b)))))));
				var modernGrid = _class + ('{' + (columns + (rows + (gapX + (gapY + '}')))));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msColumns + (msRows + '}')));
				return _List_fromArray(
					[base, supports]);
			case 'GridPosition':
				var position = rule.a;
				var msPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'-ms-grid-row: ' + ($elm$core$String$fromInt(position.row) + ';'),
							'-ms-grid-row-span: ' + ($elm$core$String$fromInt(position.height) + ';'),
							'-ms-grid-column: ' + ($elm$core$String$fromInt(position.col) + ';'),
							'-ms-grid-column-span: ' + ($elm$core$String$fromInt(position.width) + ';')
						]));
				var modernPosition = A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'grid-row: ' + ($elm$core$String$fromInt(position.row) + (' / ' + ($elm$core$String$fromInt(position.row + position.height) + ';'))),
							'grid-column: ' + ($elm$core$String$fromInt(position.col) + (' / ' + ($elm$core$String$fromInt(position.col + position.width) + ';')))
						]));
				var _class = '.grid-pos-' + ($elm$core$String$fromInt(position.row) + ('-' + ($elm$core$String$fromInt(position.col) + ('-' + ($elm$core$String$fromInt(position.width) + ('-' + $elm$core$String$fromInt(position.height)))))));
				var modernGrid = _class + ('{' + (modernPosition + '}'));
				var supports = '@supports (display:grid) {' + (modernGrid + '}');
				var base = _class + ('{' + (msPosition + '}'));
				return _List_fromArray(
					[base, supports]);
			case 'PseudoSelector':
				var _class = rule.a;
				var styles = rule.b;
				var renderPseudoRule = function (style) {
					return A3(
						$mdgriffith$elm_ui$Internal$Model$renderStyleRule,
						options,
						style,
						$elm$core$Maybe$Just(_class));
				};
				return A2($elm$core$List$concatMap, renderPseudoRule, styles);
			default:
				var transform = rule.a;
				var val = $mdgriffith$elm_ui$Internal$Model$transformValue(transform);
				var _class = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				var _v12 = _Utils_Tuple2(_class, val);
				if ((_v12.a.$ === 'Just') && (_v12.b.$ === 'Just')) {
					var cls = _v12.a.a;
					var v = _v12.b.a;
					return A4(
						$mdgriffith$elm_ui$Internal$Model$renderStyle,
						options,
						maybePseudo,
						'.' + cls,
						_List_fromArray(
							[
								A2($mdgriffith$elm_ui$Internal$Model$Property, 'transform', v)
							]));
				} else {
					return _List_Nil;
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$encodeStyles = F2(
	function (options, stylesheet) {
		return $elm$json$Json$Encode$object(
			A2(
				$elm$core$List$map,
				function (style) {
					var styled = A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing);
					return _Utils_Tuple2(
						$mdgriffith$elm_ui$Internal$Model$getStyleName(style),
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, styled));
				},
				stylesheet));
	});
var $mdgriffith$elm_ui$Internal$Model$bracket = F2(
	function (selector, rules) {
		var renderPair = function (_v0) {
			var name = _v0.a;
			var val = _v0.b;
			return name + (': ' + (val + ';'));
		};
		return selector + (' {' + (A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, renderPair, rules)) + '}'));
	});
var $mdgriffith$elm_ui$Internal$Model$fontRule = F3(
	function (name, modifier, _v0) {
		var parentAdj = _v0.a;
		var textAdjustment = _v0.b;
		return _List_fromArray(
			[
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + (', ' + ('.' + (name + (' .' + modifier))))))), parentAdj),
				A2($mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.text + (', .' + (name + (' .' + (modifier + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.text)))))))))), textAdjustment)
			]);
	});
var $mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule = F3(
	function (fontToAdjust, _v0, otherFontName) {
		var full = _v0.a;
		var capital = _v0.b;
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_Utils_ap(
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital, capital),
				A3($mdgriffith$elm_ui$Internal$Model$fontRule, name, $mdgriffith$elm_ui$Internal$Style$classes.fullSize, full)));
	});
var $mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule = F2(
	function (fontToAdjust, otherFontName) {
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital + (', ' + ('.' + (name + (' .' + $mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('line-height', '1')
						])),
					A2(
					$mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital + ('> .' + ($mdgriffith$elm_ui$Internal$Style$classes.text + (', .' + (name + (' .' + ($mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital + (' > .' + $mdgriffith$elm_ui$Internal$Style$classes.text)))))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('vertical-align', '0'),
							_Utils_Tuple2('line-height', '1')
						]))
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$adjust = F3(
	function (size, height, vertical) {
		return {height: height / size, size: size, vertical: vertical};
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $mdgriffith$elm_ui$Internal$Model$convertAdjustment = function (adjustment) {
	var lines = _List_fromArray(
		[adjustment.capital, adjustment.baseline, adjustment.descender, adjustment.lowercase]);
	var lineHeight = 1.5;
	var normalDescender = (lineHeight - 1) / 2;
	var oldMiddle = lineHeight / 2;
	var descender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.descender,
		$elm$core$List$minimum(lines));
	var newBaseline = A2(
		$elm$core$Maybe$withDefault,
		adjustment.baseline,
		$elm$core$List$minimum(
			A2(
				$elm$core$List$filter,
				function (x) {
					return !_Utils_eq(x, descender);
				},
				lines)));
	var base = lineHeight;
	var ascender = A2(
		$elm$core$Maybe$withDefault,
		adjustment.capital,
		$elm$core$List$maximum(lines));
	var capitalSize = 1 / (ascender - newBaseline);
	var capitalVertical = 1 - ascender;
	var fullSize = 1 / (ascender - descender);
	var fullVertical = 1 - ascender;
	var newCapitalMiddle = ((ascender - newBaseline) / 2) + newBaseline;
	var newFullMiddle = ((ascender - descender) / 2) + descender;
	return {
		capital: A3($mdgriffith$elm_ui$Internal$Model$adjust, capitalSize, ascender - newBaseline, capitalVertical),
		full: A3($mdgriffith$elm_ui$Internal$Model$adjust, fullSize, ascender - descender, fullVertical)
	};
};
var $mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules = function (converted) {
	return _Utils_Tuple2(
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'block')
			]),
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'inline-block'),
				_Utils_Tuple2(
				'line-height',
				$elm$core$String$fromFloat(converted.height)),
				_Utils_Tuple2(
				'vertical-align',
				$elm$core$String$fromFloat(converted.vertical) + 'em'),
				_Utils_Tuple2(
				'font-size',
				$elm$core$String$fromFloat(converted.size) + 'em')
			]));
};
var $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment = function (typefaces) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (face, found) {
				if (found.$ === 'Nothing') {
					if (face.$ === 'FontWith') {
						var _with = face.a;
						var _v2 = _with.adjustment;
						if (_v2.$ === 'Nothing') {
							return found;
						} else {
							var adjustment = _v2.a;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.full;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment))),
									$mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.capital;
										}(
											$mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment)))));
						}
					} else {
						return found;
					}
				} else {
					return found;
				}
			}),
		$elm$core$Maybe$Nothing,
		typefaces);
};
var $mdgriffith$elm_ui$Internal$Model$renderTopLevelValues = function (rules) {
	var withImport = function (font) {
		if (font.$ === 'ImportFont') {
			var url = font.b;
			return $elm$core$Maybe$Just('@import url(\'' + (url + '\');'));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	var fontImports = function (_v2) {
		var name = _v2.a;
		var typefaces = _v2.b;
		var imports = A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$filterMap, withImport, typefaces));
		return imports;
	};
	var allNames = A2($elm$core$List$map, $elm$core$Tuple$first, rules);
	var fontAdjustments = function (_v1) {
		var name = _v1.a;
		var typefaces = _v1.b;
		var _v0 = $mdgriffith$elm_ui$Internal$Model$typefaceAdjustment(typefaces);
		if (_v0.$ === 'Nothing') {
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					$mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule(name),
					allNames));
		} else {
			var adjustment = _v0.a;
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					A2($mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule, name, adjustment),
					allNames));
		}
	};
	return _Utils_ap(
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontImports, rules)),
		A2(
			$elm$core$String$join,
			'\n',
			A2($elm$core$List$map, fontAdjustments, rules)));
};
var $mdgriffith$elm_ui$Internal$Model$topLevelValue = function (rule) {
	if (rule.$ === 'FontFamily') {
		var name = rule.a;
		var typefaces = rule.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(name, typefaces));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$toStyleSheetString = F2(
	function (options, stylesheet) {
		var combine = F2(
			function (style, rendered) {
				return {
					rules: _Utils_ap(
						rendered.rules,
						A3($mdgriffith$elm_ui$Internal$Model$renderStyleRule, options, style, $elm$core$Maybe$Nothing)),
					topLevel: function () {
						var _v1 = $mdgriffith$elm_ui$Internal$Model$topLevelValue(style);
						if (_v1.$ === 'Nothing') {
							return rendered.topLevel;
						} else {
							var topLevel = _v1.a;
							return A2($elm$core$List$cons, topLevel, rendered.topLevel);
						}
					}()
				};
			});
		var _v0 = A3(
			$elm$core$List$foldl,
			combine,
			{rules: _List_Nil, topLevel: _List_Nil},
			stylesheet);
		var topLevel = _v0.topLevel;
		var rules = _v0.rules;
		return _Utils_ap(
			$mdgriffith$elm_ui$Internal$Model$renderTopLevelValues(topLevel),
			$elm$core$String$concat(rules));
	});
var $mdgriffith$elm_ui$Internal$Model$toStyleSheet = F2(
	function (options, styleSheet) {
		var _v0 = options.mode;
		switch (_v0.$) {
			case 'Layout':
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			case 'NoStaticStyleSheet':
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'div',
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$elm$virtual_dom$VirtualDom$node,
							'style',
							_List_Nil,
							_List_fromArray(
								[
									$elm$virtual_dom$VirtualDom$text(
									A2($mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
								]))
						]));
			default:
				return A3(
					$elm$virtual_dom$VirtualDom$node,
					'elm-ui-rules',
					_List_fromArray(
						[
							A2(
							$elm$virtual_dom$VirtualDom$property,
							'rules',
							A2($mdgriffith$elm_ui$Internal$Model$encodeStyles, options, styleSheet))
						]),
					_List_Nil);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$embedKeyed = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.focus)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			_Utils_Tuple2(
				'static-stylesheet',
				$mdgriffith$elm_ui$Internal$Model$staticRoot(opts)),
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
				children)) : A2(
			$elm$core$List$cons,
			_Utils_Tuple2('dynamic-stylesheet', dynamicStyleSheet),
			children);
	});
var $mdgriffith$elm_ui$Internal$Model$embedWith = F4(
	function (_static, opts, styles, children) {
		var dynamicStyleSheet = A2(
			$mdgriffith$elm_ui$Internal$Model$toStyleSheet,
			opts,
			A3(
				$elm$core$List$foldl,
				$mdgriffith$elm_ui$Internal$Model$reduceStyles,
				_Utils_Tuple2(
					$elm$core$Set$empty,
					$mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.focus)),
				styles).b);
		return _static ? A2(
			$elm$core$List$cons,
			$mdgriffith$elm_ui$Internal$Model$staticRoot(opts),
			A2($elm$core$List$cons, dynamicStyleSheet, children)) : A2($elm$core$List$cons, dynamicStyleSheet, children);
	});
var $mdgriffith$elm_ui$Internal$Flag$heightBetween = $mdgriffith$elm_ui$Internal$Flag$flag(45);
var $mdgriffith$elm_ui$Internal$Flag$heightFill = $mdgriffith$elm_ui$Internal$Flag$flag(37);
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$core$Bitwise$and = _Bitwise_and;
var $mdgriffith$elm_ui$Internal$Flag$present = F2(
	function (myFlag, _v0) {
		var fieldOne = _v0.a;
		var fieldTwo = _v0.b;
		if (myFlag.$ === 'Flag') {
			var first = myFlag.a;
			return _Utils_eq(first & fieldOne, first);
		} else {
			var second = myFlag.a;
			return _Utils_eq(second & fieldTwo, second);
		}
	});
var $elm$html$Html$s = _VirtualDom_node('s');
var $elm$html$Html$u = _VirtualDom_node('u');
var $mdgriffith$elm_ui$Internal$Flag$widthBetween = $mdgriffith$elm_ui$Internal$Flag$flag(44);
var $mdgriffith$elm_ui$Internal$Flag$widthFill = $mdgriffith$elm_ui$Internal$Flag$flag(39);
var $mdgriffith$elm_ui$Internal$Model$finalizeNode = F6(
	function (has, node, attributes, children, embedMode, parentContext) {
		var createNode = F2(
			function (nodeName, attrs) {
				if (children.$ === 'Keyed') {
					var keyed = children.a;
					return A3(
						$elm$virtual_dom$VirtualDom$keyedNode,
						nodeName,
						attrs,
						function () {
							switch (embedMode.$) {
								case 'NoStyleSheet':
									return keyed;
								case 'OnlyDynamic':
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, false, opts, styles, keyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedKeyed, true, opts, styles, keyed);
							}
						}());
				} else {
					var unkeyed = children.a;
					return A2(
						function () {
							switch (nodeName) {
								case 'div':
									return $elm$html$Html$div;
								case 'p':
									return $elm$html$Html$p;
								default:
									return $elm$virtual_dom$VirtualDom$node(nodeName);
							}
						}(),
						attrs,
						function () {
							switch (embedMode.$) {
								case 'NoStyleSheet':
									return unkeyed;
								case 'OnlyDynamic':
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, false, opts, styles, unkeyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4($mdgriffith$elm_ui$Internal$Model$embedWith, true, opts, styles, unkeyed);
							}
						}());
				}
			});
		var html = function () {
			switch (node.$) {
				case 'Generic':
					return A2(createNode, 'div', attributes);
				case 'NodeName':
					var nodeName = node.a;
					return A2(createNode, nodeName, attributes);
				default:
					var nodeName = node.a;
					var internal = node.b;
					return A3(
						$elm$virtual_dom$VirtualDom$node,
						nodeName,
						attributes,
						_List_fromArray(
							[
								A2(
								createNode,
								internal,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.single))
									]))
							]));
			}
		}();
		switch (parentContext.$) {
			case 'AsRow':
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$widthBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignRight, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.any, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.container, $mdgriffith$elm_ui$Internal$Style$classes.contentCenterY, $mdgriffith$elm_ui$Internal$Style$classes.alignContainerRight])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerX, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.any, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.container, $mdgriffith$elm_ui$Internal$Style$classes.contentCenterY, $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX])))
						]),
					_List_fromArray(
						[html])) : html));
			case 'AsColumn':
				return (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightFill, has) && (!A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$heightBetween, has))) ? html : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$centerY, has) ? A2(
					$elm$html$Html$s,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.any, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.container, $mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY])))
						]),
					_List_fromArray(
						[html])) : (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$alignBottom, has) ? A2(
					$elm$html$Html$u,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[$mdgriffith$elm_ui$Internal$Style$classes.any, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.container, $mdgriffith$elm_ui$Internal$Style$classes.alignContainerBottom])))
						]),
					_List_fromArray(
						[html])) : html));
			default:
				return html;
		}
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $mdgriffith$elm_ui$Internal$Model$textElementClasses = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.text + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.widthContent + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.heightContent)))));
var $mdgriffith$elm_ui$Internal$Model$textElement = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$textElementFillClasses = $mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.text + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.widthFill + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.heightFill)))));
var $mdgriffith$elm_ui$Internal$Model$textElementFill = function (str) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Model$textElementFillClasses)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $mdgriffith$elm_ui$Internal$Model$createElement = F3(
	function (context, children, rendered) {
		var gatherKeyed = F2(
			function (_v8, _v9) {
				var key = _v8.a;
				var child = _v8.b;
				var htmls = _v9.a;
				var existingStyles = _v9.b;
				switch (child.$) {
					case 'Unstyled':
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles);
					case 'Styled':
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.html, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.html, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles));
					case 'Text':
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									key,
									_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str)),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		var gather = F2(
			function (child, _v6) {
				var htmls = _v6.a;
				var existingStyles = _v6.b;
				switch (child.$) {
					case 'Unstyled':
						var html = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								html(context),
								htmls),
							existingStyles);
					case 'Styled':
						var styled = child.a;
						return _Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.html, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles)) : _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(styled.html, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							$elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles));
					case 'Text':
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								_Utils_eq(context, $mdgriffith$elm_ui$Internal$Model$asEl) ? $mdgriffith$elm_ui$Internal$Model$textElementFill(str) : $mdgriffith$elm_ui$Internal$Model$textElement(str),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		if (children.$ === 'Keyed') {
			var keyedChildren = children.a;
			var _v1 = A3(
				$elm$core$List$foldr,
				gatherKeyed,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				keyedChildren);
			var keyed = _v1.a;
			var styles = _v1.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.styles : _Utils_ap(rendered.styles, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.has,
						rendered.node,
						rendered.attributes,
						$mdgriffith$elm_ui$Internal$Model$Keyed(
							A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.children)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						html: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.has,
							rendered.node,
							rendered.attributes,
							$mdgriffith$elm_ui$Internal$Model$Keyed(
								A3($mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.children))),
						styles: allStyles
					});
			}
		} else {
			var unkeyedChildren = children.a;
			var _v3 = A3(
				$elm$core$List$foldr,
				gather,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				unkeyedChildren);
			var unkeyed = _v3.a;
			var styles = _v3.b;
			var newStyles = $elm$core$List$isEmpty(styles) ? rendered.styles : _Utils_ap(rendered.styles, styles);
			if (!newStyles.b) {
				return $mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						$mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.has,
						rendered.node,
						rendered.attributes,
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.children)),
						$mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return $mdgriffith$elm_ui$Internal$Model$Styled(
					{
						html: A4(
							$mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.has,
							rendered.node,
							rendered.attributes,
							$mdgriffith$elm_ui$Internal$Model$Unkeyed(
								A2($mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.children))),
						styles: allStyles
					});
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Single = F3(
	function (a, b, c) {
		return {$: 'Single', a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$Transform = function (a) {
	return {$: 'Transform', a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $mdgriffith$elm_ui$Internal$Flag$add = F2(
	function (myFlag, _v0) {
		var one = _v0.a;
		var two = _v0.b;
		if (myFlag.$ === 'Flag') {
			var first = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, first | one, two);
		} else {
			var second = myFlag.a;
			return A2($mdgriffith$elm_ui$Internal$Flag$Field, one, second | two);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehind = function (a) {
	return {$: 'ChildrenBehind', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront = F2(
	function (a, b) {
		return {$: 'ChildrenBehindAndInFront', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$ChildrenInFront = function (a) {
	return {$: 'ChildrenInFront', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$nearbyElement = F2(
	function (location, elem) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(
					function () {
						switch (location.$) {
							case 'Above':
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.above]));
							case 'Below':
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.below]));
							case 'OnRight':
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.onRight]));
							case 'OnLeft':
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.onLeft]));
							case 'InFront':
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.inFront]));
							default:
								return A2(
									$elm$core$String$join,
									' ',
									_List_fromArray(
										[$mdgriffith$elm_ui$Internal$Style$classes.nearby, $mdgriffith$elm_ui$Internal$Style$classes.single, $mdgriffith$elm_ui$Internal$Style$classes.behind]));
						}
					}())
				]),
			_List_fromArray(
				[
					function () {
					switch (elem.$) {
						case 'Empty':
							return $elm$virtual_dom$VirtualDom$text('');
						case 'Text':
							var str = elem.a;
							return $mdgriffith$elm_ui$Internal$Model$textElement(str);
						case 'Unstyled':
							var html = elem.a;
							return html($mdgriffith$elm_ui$Internal$Model$asEl);
						default:
							var styled = elem.a;
							return A2(styled.html, $mdgriffith$elm_ui$Internal$Model$NoStyleSheet, $mdgriffith$elm_ui$Internal$Model$asEl);
					}
				}()
				]));
	});
var $mdgriffith$elm_ui$Internal$Model$addNearbyElement = F3(
	function (location, elem, existing) {
		var nearby = A2($mdgriffith$elm_ui$Internal$Model$nearbyElement, location, elem);
		switch (existing.$) {
			case 'NoNearbyChildren':
				if (location.$ === 'Behind') {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						_List_fromArray(
							[nearby]));
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						_List_fromArray(
							[nearby]));
				}
			case 'ChildrenBehind':
				var existingBehind = existing.a;
				if (location.$ === 'Behind') {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						A2($elm$core$List$cons, nearby, existingBehind));
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						_List_fromArray(
							[nearby]));
				}
			case 'ChildrenInFront':
				var existingInFront = existing.a;
				if (location.$ === 'Behind') {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						_List_fromArray(
							[nearby]),
						existingInFront);
				} else {
					return $mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						A2($elm$core$List$cons, nearby, existingInFront));
				}
			default:
				var existingBehind = existing.a;
				var existingInFront = existing.b;
				if (location.$ === 'Behind') {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						A2($elm$core$List$cons, nearby, existingBehind),
						existingInFront);
				} else {
					return A2(
						$mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						A2($elm$core$List$cons, nearby, existingInFront));
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Embedded = F2(
	function (a, b) {
		return {$: 'Embedded', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$NodeName = function (a) {
	return {$: 'NodeName', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$addNodeName = F2(
	function (newNode, old) {
		switch (old.$) {
			case 'Generic':
				return $mdgriffith$elm_ui$Internal$Model$NodeName(newNode);
			case 'NodeName':
				var name = old.a;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, name, newNode);
			default:
				var x = old.a;
				var y = old.b;
				return A2($mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
		}
	});
var $mdgriffith$elm_ui$Internal$Model$alignXName = function (align) {
	switch (align.$) {
		case 'Left':
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedHorizontally + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignLeft);
		case 'Right':
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedHorizontally + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignRight);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedHorizontally + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignCenterX);
	}
};
var $mdgriffith$elm_ui$Internal$Model$alignYName = function (align) {
	switch (align.$) {
		case 'Top':
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedVertically + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignTop);
		case 'Bottom':
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedVertically + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignBottom);
		default:
			return $mdgriffith$elm_ui$Internal$Style$classes.alignedVertically + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.alignCenterY);
	}
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $mdgriffith$elm_ui$Internal$Model$FullTransform = F4(
	function (a, b, c, d) {
		return {$: 'FullTransform', a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Internal$Model$Moved = function (a) {
	return {$: 'Moved', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$composeTransformation = F2(
	function (transform, component) {
		switch (transform.$) {
			case 'Untransformed':
				switch (component.$) {
					case 'MoveX':
						var x = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, 0, 0));
					case 'MoveY':
						var y = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, y, 0));
					case 'MoveZ':
						var z = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, 0, z));
					case 'MoveXYZ':
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 'Rotate':
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var xyz = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							xyz,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			case 'Moved':
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				switch (component.$) {
					case 'MoveX':
						var newX = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(newX, y, z));
					case 'MoveY':
						var newY = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, newY, z));
					case 'MoveZ':
						var newZ = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, y, newZ));
					case 'MoveXYZ':
						var xyz = component.a;
						return $mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 'Rotate':
						var xyz = component.a;
						var angle = component.b;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var scale = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							scale,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			default:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				var scaled = transform.b;
				var origin = transform.c;
				var angle = transform.d;
				switch (component.$) {
					case 'MoveX':
						var newX = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(newX, y, z),
							scaled,
							origin,
							angle);
					case 'MoveY':
						var newY = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, newY, z),
							scaled,
							origin,
							angle);
					case 'MoveZ':
						var newZ = component.a;
						return A4(
							$mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, y, newZ),
							scaled,
							origin,
							angle);
					case 'MoveXYZ':
						var newMove = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, newMove, scaled, origin, angle);
					case 'Rotate':
						var newOrigin = component.a;
						var newAngle = component.b;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, scaled, newOrigin, newAngle);
					default:
						var newScale = component.a;
						return A4($mdgriffith$elm_ui$Internal$Model$FullTransform, moved, newScale, origin, angle);
				}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$height = $mdgriffith$elm_ui$Internal$Flag$flag(7);
var $mdgriffith$elm_ui$Internal$Flag$heightContent = $mdgriffith$elm_ui$Internal$Flag$flag(36);
var $mdgriffith$elm_ui$Internal$Flag$merge = F2(
	function (_v0, _v1) {
		var one = _v0.a;
		var two = _v0.b;
		var three = _v1.a;
		var four = _v1.b;
		return A2($mdgriffith$elm_ui$Internal$Flag$Field, one | three, two | four);
	});
var $mdgriffith$elm_ui$Internal$Flag$none = A2($mdgriffith$elm_ui$Internal$Flag$Field, 0, 0);
var $mdgriffith$elm_ui$Internal$Model$renderHeight = function (h) {
	switch (h.$) {
		case 'Px':
			var px = h.a;
			var val = $elm$core$String$fromInt(px);
			var name = 'height-px-' + val;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.heightExact + (' ' + name),
				_List_fromArray(
					[
						A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height', val + 'px')
					]));
		case 'Content':
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.heightContent,
				_List_Nil);
		case 'Fill':
			var portion = h.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.heightFill,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.heightFillPortion + (' height-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.column + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'height-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 'Min':
			var minSize = h.a;
			var len = h.b;
			var cls = 'min-height-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-height',
				$elm$core$String$fromInt(minSize) + 'px !important');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = h.a;
			var len = h.b;
			var cls = 'max-height-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-height',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$widthContent = $mdgriffith$elm_ui$Internal$Flag$flag(38);
var $mdgriffith$elm_ui$Internal$Model$renderWidth = function (w) {
	switch (w.$) {
		case 'Px':
			var px = w.a;
			return _Utils_Tuple3(
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Style$classes.widthExact + (' width-px-' + $elm$core$String$fromInt(px)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						'width-px-' + $elm$core$String$fromInt(px),
						'width',
						$elm$core$String$fromInt(px) + 'px')
					]));
		case 'Content':
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthContent, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.widthContent,
				_List_Nil);
		case 'Fill':
			var portion = w.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.widthFill,
				_List_Nil) : _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthFill, $mdgriffith$elm_ui$Internal$Flag$none),
				$mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion + (' width-fill-' + $elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						$mdgriffith$elm_ui$Internal$Model$Single,
						$mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.row + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
							'width-fill-' + $elm$core$String$fromInt(portion))))),
						'flex-grow',
						$elm$core$String$fromInt(portion * 100000))
					]));
		case 'Min':
			var minSize = w.a;
			var len = w.b;
			var cls = 'min-width-' + $elm$core$String$fromInt(minSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-width',
				$elm$core$String$fromInt(minSize) + 'px');
			var _v1 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v1.a;
			var newAttrs = _v1.b;
			var newStyle = _v1.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
		default:
			var maxSize = w.a;
			var len = w.b;
			var cls = 'max-width-' + $elm$core$String$fromInt(maxSize);
			var style = A3(
				$mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-width',
				$elm$core$String$fromInt(maxSize) + 'px');
			var _v2 = $mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _v2.a;
			var newAttrs = _v2.b;
			var newStyle = _v2.c;
			return _Utils_Tuple3(
				A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2($elm$core$List$cons, style, newStyle));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$borderWidth = $mdgriffith$elm_ui$Internal$Flag$flag(27);
var $mdgriffith$elm_ui$Internal$Model$skippable = F2(
	function (flag, style) {
		if (_Utils_eq(flag, $mdgriffith$elm_ui$Internal$Flag$borderWidth)) {
			if (style.$ === 'Single') {
				var val = style.c;
				switch (val) {
					case '0px':
						return true;
					case '1px':
						return true;
					case '2px':
						return true;
					case '3px':
						return true;
					case '4px':
						return true;
					case '5px':
						return true;
					case '6px':
						return true;
					default:
						return false;
				}
			} else {
				return false;
			}
		} else {
			switch (style.$) {
				case 'FontSize':
					var i = style.a;
					return (i >= 8) && (i <= 32);
				case 'PaddingStyle':
					var name = style.a;
					var t = style.b;
					var r = style.c;
					var b = style.d;
					var l = style.e;
					return _Utils_eq(t, b) && (_Utils_eq(t, r) && (_Utils_eq(t, l) && ((t >= 0) && (t <= 24))));
				default:
					return false;
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Flag$width = $mdgriffith$elm_ui$Internal$Flag$flag(6);
var $mdgriffith$elm_ui$Internal$Flag$xAlign = $mdgriffith$elm_ui$Internal$Flag$flag(30);
var $mdgriffith$elm_ui$Internal$Flag$yAlign = $mdgriffith$elm_ui$Internal$Flag$flag(29);
var $mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive = F8(
	function (classes, node, has, transform, styles, attrs, children, elementAttrs) {
		gatherAttrRecursive:
		while (true) {
			if (!elementAttrs.b) {
				var _v1 = $mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				if (_v1.$ === 'Nothing') {
					return {
						attributes: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes),
							attrs),
						children: children,
						has: has,
						node: node,
						styles: styles
					};
				} else {
					var _class = _v1.a;
					return {
						attributes: A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class(classes + (' ' + _class)),
							attrs),
						children: children,
						has: has,
						node: node,
						styles: A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$Transform(transform),
							styles)
					};
				}
			} else {
				var attribute = elementAttrs.a;
				var remaining = elementAttrs.b;
				switch (attribute.$) {
					case 'NoAttribute':
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'Class':
						var flag = attribute.a;
						var exactClassName = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = exactClassName + (' ' + classes),
								$temp$node = node,
								$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					case 'Attr':
						var actualAttribute = attribute.a;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = A2($elm$core$List$cons, actualAttribute, attrs),
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'StyleClass':
						var flag = attribute.a;
						var style = attribute.b;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							if (A2($mdgriffith$elm_ui$Internal$Model$skippable, flag, style)) {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							} else {
								var $temp$classes = $mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = A2($elm$core$List$cons, style, styles),
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							}
						}
					case 'TransformComponent':
						var flag = attribute.a;
						var component = attribute.b;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, flag, has),
							$temp$transform = A2($mdgriffith$elm_ui$Internal$Model$composeTransformation, transform, component),
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'Width':
						var width = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$width, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (width.$) {
								case 'Px':
									var px = width.a;
									var $temp$classes = ($mdgriffith$elm_ui$Internal$Style$classes.widthExact + (' width-px-' + $elm$core$String$fromInt(px))) + (' ' + classes),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3(
											$mdgriffith$elm_ui$Internal$Model$Single,
											'width-px-' + $elm$core$String$fromInt(px),
											'width',
											$elm$core$String$fromInt(px) + 'px'),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Content':
									var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.widthContent),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$widthContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Fill':
									var portion = width.a;
									if (portion === 1) {
										var $temp$classes = classes + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.widthFill),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion + (' width-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.row + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'width-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v4 = $mdgriffith$elm_ui$Internal$Model$renderWidth(width);
									var addToFlags = _v4.a;
									var newClass = _v4.b;
									var newStyles = _v4.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 'Height':
						var height = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$height, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (height.$) {
								case 'Px':
									var px = height.a;
									var val = $elm$core$String$fromInt(px) + 'px';
									var name = 'height-px-' + val;
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.heightExact + (' ' + (name + (' ' + classes))),
										$temp$node = node,
										$temp$has = A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has),
										$temp$transform = transform,
										$temp$styles = A2(
										$elm$core$List$cons,
										A3($mdgriffith$elm_ui$Internal$Model$Single, name, 'height ', val),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Content':
									var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.heightContent + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$add,
										$mdgriffith$elm_ui$Internal$Flag$heightContent,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Fill':
									var portion = height.a;
									if (portion === 1) {
										var $temp$classes = $mdgriffith$elm_ui$Internal$Style$classes.heightFill + (' ' + classes),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.heightFillPortion + (' height-fill-' + $elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											$mdgriffith$elm_ui$Internal$Flag$add,
											$mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											$elm$core$List$cons,
											A3(
												$mdgriffith$elm_ui$Internal$Model$Single,
												$mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + ($mdgriffith$elm_ui$Internal$Style$classes.column + (' > ' + $mdgriffith$elm_ui$Internal$Style$dot(
													'height-fill-' + $elm$core$String$fromInt(portion))))),
												'flex-grow',
												$elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _v6 = $mdgriffith$elm_ui$Internal$Model$renderHeight(height);
									var addToFlags = _v6.a;
									var newClass = _v6.b;
									var newStyles = _v6.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(
										$mdgriffith$elm_ui$Internal$Flag$merge,
										addToFlags,
										A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 'Describe':
						var description = attribute.a;
						switch (description.$) {
							case 'Main':
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'main', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Navigation':
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'nav', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'ContentInfo':
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'footer', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Complementary':
								var $temp$classes = classes,
									$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'aside', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Heading':
								var i = description.a;
								if (i <= 1) {
									var $temp$classes = classes,
										$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h1', node),
										$temp$has = has,
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								} else {
									if (i < 7) {
										var $temp$classes = classes,
											$temp$node = A2(
											$mdgriffith$elm_ui$Internal$Model$addNodeName,
											'h' + $elm$core$String$fromInt(i),
											node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes,
											$temp$node = A2($mdgriffith$elm_ui$Internal$Model$addNodeName, 'h6', node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								}
							case 'Paragraph':
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Button':
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'role', 'button'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Label':
								var label = description.a;
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-label', label),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'LivePolite':
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'polite'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							default:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									$elm$core$List$cons,
									A2($elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'assertive'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
						}
					case 'Nearby':
						var location = attribute.a;
						var elem = attribute.b;
						var newStyles = function () {
							switch (elem.$) {
								case 'Empty':
									return styles;
								case 'Text':
									var str = elem.a;
									return styles;
								case 'Unstyled':
									var html = elem.a;
									return styles;
								default:
									var styled = elem.a;
									return _Utils_ap(styles, styled.styles);
							}
						}();
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = newStyles,
							$temp$attrs = attrs,
							$temp$children = A3($mdgriffith$elm_ui$Internal$Model$addNearbyElement, location, elem, children),
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'AlignX':
						var x = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignXName(x) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (x.$) {
									case 'CenterX':
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerX, flags);
									case 'Right':
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignRight, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$xAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					default:
						var y = attribute.a;
						if (A2($mdgriffith$elm_ui$Internal$Flag$present, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = $mdgriffith$elm_ui$Internal$Model$alignYName(y) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (y.$) {
									case 'CenterY':
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$centerY, flags);
									case 'Bottom':
										return A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$alignBottom, flags);
									default:
										return flags;
								}
							}(
								A2($mdgriffith$elm_ui$Internal$Flag$add, $mdgriffith$elm_ui$Internal$Flag$yAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
				}
			}
		}
	});
var $mdgriffith$elm_ui$Internal$Model$Untransformed = {$: 'Untransformed'};
var $mdgriffith$elm_ui$Internal$Model$untransformed = $mdgriffith$elm_ui$Internal$Model$Untransformed;
var $mdgriffith$elm_ui$Internal$Model$element = F4(
	function (context, node, attributes, children) {
		return A3(
			$mdgriffith$elm_ui$Internal$Model$createElement,
			context,
			children,
			A8(
				$mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive,
				$mdgriffith$elm_ui$Internal$Model$contextClasses(context),
				node,
				$mdgriffith$elm_ui$Internal$Flag$none,
				$mdgriffith$elm_ui$Internal$Model$untransformed,
				_List_Nil,
				_List_Nil,
				$mdgriffith$elm_ui$Internal$Model$NoNearbyChildren,
				$elm$core$List$reverse(attributes)));
	});
var $mdgriffith$elm_ui$Element$Input$enter = 'Enter';
var $mdgriffith$elm_ui$Internal$Model$NoAttribute = {$: 'NoAttribute'};
var $mdgriffith$elm_ui$Element$Input$hasFocusStyle = function (attr) {
	if (((attr.$ === 'StyleClass') && (attr.b.$ === 'PseudoSelector')) && (attr.b.a.$ === 'Focus')) {
		var _v1 = attr.b;
		var _v2 = _v1.a;
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Internal$Model$htmlClass = function (cls) {
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		$elm$html$Html$Attributes$class(cls));
};
var $mdgriffith$elm_ui$Element$Input$focusDefault = function (attrs) {
	return A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, attrs) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass('focusable');
};
var $mdgriffith$elm_ui$Internal$Model$Height = function (a) {
	return {$: 'Height', a: a};
};
var $mdgriffith$elm_ui$Element$height = $mdgriffith$elm_ui$Internal$Model$Height;
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $mdgriffith$elm_ui$Element$Events$onClick = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Events$onClick);
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 'MayPreventDefault', a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $mdgriffith$elm_ui$Element$Input$onKeyLookup = function (lookup) {
	var decode = function (code) {
		var _v0 = lookup(code);
		if (_v0.$ === 'Nothing') {
			return $elm$json$Json$Decode$fail('No key matched');
		} else {
			var msg = _v0.a;
			return $elm$json$Json$Decode$succeed(msg);
		}
	};
	var isKey = A2(
		$elm$json$Json$Decode$andThen,
		decode,
		A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
	return $mdgriffith$elm_ui$Internal$Model$Attr(
		A2(
			$elm$html$Html$Events$preventDefaultOn,
			'keydown',
			A2(
				$elm$json$Json$Decode$map,
				function (fired) {
					return _Utils_Tuple2(fired, true);
				},
				isKey)));
};
var $mdgriffith$elm_ui$Internal$Model$Class = F2(
	function (a, b) {
		return {$: 'Class', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$cursor = $mdgriffith$elm_ui$Internal$Flag$flag(21);
var $mdgriffith$elm_ui$Element$pointer = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.cursorPointer);
var $mdgriffith$elm_ui$Internal$Model$Content = {$: 'Content'};
var $mdgriffith$elm_ui$Element$shrink = $mdgriffith$elm_ui$Internal$Model$Content;
var $mdgriffith$elm_ui$Element$Input$space = ' ';
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $mdgriffith$elm_ui$Internal$Model$Width = function (a) {
	return {$: 'Width', a: a};
};
var $mdgriffith$elm_ui$Element$width = $mdgriffith$elm_ui$Internal$Model$Width;
var $mdgriffith$elm_ui$Element$Input$button = F2(
	function (attrs, _v0) {
		var onPress = _v0.onPress;
		var label = _v0.label;
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.contentCenterX + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.contentCenterY + (' ' + ($mdgriffith$elm_ui$Internal$Style$classes.seButton + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.noTextSelection)))))),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Element$pointer,
							A2(
								$elm$core$List$cons,
								$mdgriffith$elm_ui$Element$Input$focusDefault(attrs),
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$Button),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Internal$Model$Attr(
											$elm$html$Html$Attributes$tabindex(0)),
										function () {
											if (onPress.$ === 'Nothing') {
												return A2(
													$elm$core$List$cons,
													$mdgriffith$elm_ui$Internal$Model$Attr(
														$elm$html$Html$Attributes$disabled(true)),
													attrs);
											} else {
												var msg = onPress.a;
												return A2(
													$elm$core$List$cons,
													$mdgriffith$elm_ui$Element$Events$onClick(msg),
													A2(
														$elm$core$List$cons,
														$mdgriffith$elm_ui$Element$Input$onKeyLookup(
															function (code) {
																return _Utils_eq(code, $mdgriffith$elm_ui$Element$Input$enter) ? $elm$core$Maybe$Just(msg) : (_Utils_eq(code, $mdgriffith$elm_ui$Element$Input$space) ? $elm$core$Maybe$Just(msg) : $elm$core$Maybe$Nothing);
															}),
														attrs));
											}
										}()))))))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var $mdgriffith$elm_ui$Internal$Model$Colored = F3(
	function (a, b, c) {
		return {$: 'Colored', a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Model$StyleClass = F2(
	function (a, b) {
		return {$: 'StyleClass', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$fontColor = $mdgriffith$elm_ui$Internal$Flag$flag(14);
var $mdgriffith$elm_ui$Internal$Model$formatColorClass = function (_v0) {
	var red = _v0.a;
	var green = _v0.b;
	var blue = _v0.c;
	var alpha = _v0.d;
	return $mdgriffith$elm_ui$Internal$Model$floatClass(red) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(green) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(blue) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(alpha))))));
};
var $mdgriffith$elm_ui$Element$Font$color = function (fontColor) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(fontColor),
			'color',
			fontColor));
};
var $mdgriffith$elm_ui$Element$el = F2(
	function (attrs, child) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					attrs)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[child])));
	});
var $mdgriffith$elm_ui$Internal$Model$PaddingStyle = F5(
	function (a, b, c, d, e) {
		return {$: 'PaddingStyle', a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Internal$Flag$padding = $mdgriffith$elm_ui$Internal$Flag$flag(2);
var $mdgriffith$elm_ui$Element$padding = function (x) {
	var f = x;
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			'p-' + $elm$core$String$fromInt(x),
			f,
			f,
			f,
			f));
};
var $mdgriffith$elm_ui$Internal$Model$Rgba = F4(
	function (a, b, c, d) {
		return {$: 'Rgba', a: a, b: b, c: c, d: d};
	});
var $mdgriffith$elm_ui$Element$rgb255 = F3(
	function (red, green, blue) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, red / 255, green / 255, blue / 255, 1);
	});
var $mdgriffith$elm_ui$Internal$Flag$borderRound = $mdgriffith$elm_ui$Internal$Flag$flag(17);
var $mdgriffith$elm_ui$Element$Border$rounded = function (radius) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderRound,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Single,
			'br-' + $elm$core$String$fromInt(radius),
			'border-radius',
			$elm$core$String$fromInt(radius) + 'px'));
};
var $mdgriffith$elm_ui$Internal$Model$FontSize = function (a) {
	return {$: 'FontSize', a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$fontSize = $mdgriffith$elm_ui$Internal$Flag$flag(4);
var $mdgriffith$elm_ui$Element$Font$size = function (i) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontSize,
		$mdgriffith$elm_ui$Internal$Model$FontSize(i));
};
var $mdgriffith$elm_ui$Internal$Model$VariantActive = function (a) {
	return {$: 'VariantActive', a: a};
};
var $mdgriffith$elm_ui$Element$Font$smallCaps = $mdgriffith$elm_ui$Internal$Model$VariantActive('smcp');
var $mdgriffith$elm_ui$Internal$Model$Text = function (a) {
	return {$: 'Text', a: a};
};
var $mdgriffith$elm_ui$Element$text = function (content) {
	return $mdgriffith$elm_ui$Internal$Model$Text(content);
};
var $mdgriffith$elm_ui$Internal$Flag$fontVariant = $mdgriffith$elm_ui$Internal$Flag$flag(48);
var $mdgriffith$elm_ui$Element$Font$variant = function (_var) {
	switch (_var.$) {
		case 'VariantActive':
			var name = _var.a;
			return A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontVariant, 'v-' + name);
		case 'VariantOff':
			var name = _var.a;
			return A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontVariant, 'v-' + (name + '-off'));
		default:
			var name = _var.a;
			var index = _var.b;
			return A2(
				$mdgriffith$elm_ui$Internal$Model$StyleClass,
				$mdgriffith$elm_ui$Internal$Flag$fontVariant,
				A3(
					$mdgriffith$elm_ui$Internal$Model$Single,
					'v-' + (name + ('-' + $elm$core$String$fromInt(index))),
					'font-feature-settings',
					'\"' + (name + ('\" ' + $elm$core$String$fromInt(index)))));
	}
};
var $mdgriffith$elm_ui$Internal$Model$BorderWidth = F5(
	function (a, b, c, d, e) {
		return {$: 'BorderWidth', a: a, b: b, c: c, d: d, e: e};
	});
var $mdgriffith$elm_ui$Element$Border$width = function (v) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			$mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + $elm$core$String$fromInt(v),
			v,
			v,
			v,
			v));
};
var $author$project$UI$button = F2(
	function (str, maybeMsg) {
		return A2(
			$mdgriffith$elm_ui$Element$Input$button,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$Font$size(16),
					$mdgriffith$elm_ui$Element$Font$variant($mdgriffith$elm_ui$Element$Font$smallCaps),
					$mdgriffith$elm_ui$Element$Font$color(
					A3($mdgriffith$elm_ui$Element$rgb255, 30, 30, 30)),
					$mdgriffith$elm_ui$Element$padding(10),
					$mdgriffith$elm_ui$Element$Border$width(1),
					$mdgriffith$elm_ui$Element$Border$rounded(3)
				]),
			{
				label: A2(
					$mdgriffith$elm_ui$Element$el,
					_List_Nil,
					$mdgriffith$elm_ui$Element$text(str)),
				onPress: maybeMsg
			});
	});
var $mdgriffith$elm_ui$Internal$Model$CenterX = {$: 'CenterX'};
var $mdgriffith$elm_ui$Element$centerX = $mdgriffith$elm_ui$Internal$Model$AlignX($mdgriffith$elm_ui$Internal$Model$CenterX);
var $mdgriffith$elm_ui$Internal$Model$CenterY = {$: 'CenterY'};
var $mdgriffith$elm_ui$Element$centerY = $mdgriffith$elm_ui$Internal$Model$AlignY($mdgriffith$elm_ui$Internal$Model$CenterY);
var $author$project$MyCharts$fillWithMaybe = F2(
	function (model, index) {
		if (!model.b) {
			return _List_Nil;
		} else {
			var x = model.a;
			var xs = model.b;
			var newIndex = index + 1;
			var chartID = function ($) {
				return $.chartID;
			}(x);
			return _Utils_eq(index, chartID) ? A2(
				$elm$core$List$cons,
				$elm$core$Maybe$Just(x),
				A2($author$project$MyCharts$fillWithMaybe, xs, newIndex)) : ((_Utils_cmp(index, chartID) > 0) ? _List_Nil : A2(
				$elm$core$List$cons,
				$elm$core$Maybe$Nothing,
				A2($author$project$MyCharts$fillWithMaybe, model, newIndex)));
		}
	});
var $author$project$MyCharts$AddChart = function (a) {
	return {$: 'AddChart', a: a};
};
var $mdgriffith$elm_ui$Internal$Flag$fontAlignment = $mdgriffith$elm_ui$Internal$Flag$flag(12);
var $mdgriffith$elm_ui$Element$Font$center = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontAlignment, $mdgriffith$elm_ui$Internal$Style$classes.textCenter);
var $mdgriffith$elm_ui$Internal$Flag$borderColor = $mdgriffith$elm_ui$Internal$Flag$flag(28);
var $mdgriffith$elm_ui$Element$Border$color = function (clr) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'bc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'border-color',
			clr));
};
var $mdgriffith$elm_ui$Internal$Model$Px = function (a) {
	return {$: 'Px', a: a};
};
var $mdgriffith$elm_ui$Element$px = $mdgriffith$elm_ui$Internal$Model$Px;
var $author$project$UI$element = F2(
	function (attributes, el) {
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_Utils_ap(
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$width(
						$mdgriffith$elm_ui$Element$px($author$project$UI$elementWidth)),
						$mdgriffith$elm_ui$Element$height(
						$mdgriffith$elm_ui$Element$px($author$project$UI$elementHeight))
					]),
				attributes),
			el);
	});
var $mdgriffith$elm_ui$Internal$Model$MoveY = function (a) {
	return {$: 'MoveY', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$TransformComponent = F2(
	function (a, b) {
		return {$: 'TransformComponent', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$moveY = $mdgriffith$elm_ui$Internal$Flag$flag(26);
var $mdgriffith$elm_ui$Element$moveUp = function (y) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$TransformComponent,
		$mdgriffith$elm_ui$Internal$Flag$moveY,
		$mdgriffith$elm_ui$Internal$Model$MoveY(-y));
};
var $author$project$UI$addNewElementSpace = function (msg) {
	return A2(
		$author$project$UI$element,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$Border$width(1),
				$mdgriffith$elm_ui$Element$Border$rounded(30),
				$mdgriffith$elm_ui$Element$Border$color(
				A3($mdgriffith$elm_ui$Element$rgb255, 150, 150, 150))
			]),
		A2(
			$mdgriffith$elm_ui$Element$Input$button,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$centerX,
					$mdgriffith$elm_ui$Element$centerY,
					$mdgriffith$elm_ui$Element$Border$color(
					A3($mdgriffith$elm_ui$Element$rgb255, 100, 100, 100)),
					$mdgriffith$elm_ui$Element$Border$width(1),
					$mdgriffith$elm_ui$Element$Border$rounded(25),
					$mdgriffith$elm_ui$Element$Font$size(30),
					$mdgriffith$elm_ui$Element$Font$center,
					$mdgriffith$elm_ui$Element$width(
					$mdgriffith$elm_ui$Element$px(50)),
					$mdgriffith$elm_ui$Element$height(
					$mdgriffith$elm_ui$Element$px(50)),
					$mdgriffith$elm_ui$Element$Font$color(
					A3($mdgriffith$elm_ui$Element$rgb255, 100, 100, 100))
				]),
			{
				label: A2(
					$mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$centerY,
							$mdgriffith$elm_ui$Element$centerX,
							$mdgriffith$elm_ui$Element$Font$center,
							$mdgriffith$elm_ui$Element$moveUp(2)
						]),
					$mdgriffith$elm_ui$Element$text('+')),
				onPress: $elm$core$Maybe$Just(msg)
			}));
};
var $author$project$MyCharts$Chart$ToggleEditCurve = {$: 'ToggleEditCurve'};
var $author$project$MyCharts$buttonToggleChart = F2(
	function (editingCurves, chartMsgToMainMsg) {
		var text = function () {
			if (editingCurves) {
				return 'Show Charts';
			} else {
				return 'Edit Curves';
			}
		}();
		return A2(
			$mdgriffith$elm_ui$Element$Input$button,
			_List_fromArray(
				[$mdgriffith$elm_ui$Element$alignRight]),
			{
				label: A2(
					$mdgriffith$elm_ui$Element$el,
					_List_Nil,
					$mdgriffith$elm_ui$Element$text(text)),
				onPress: $elm$core$Maybe$Just(
					chartMsgToMainMsg($author$project$MyCharts$Chart$ToggleEditCurve))
			});
	});
var $mdgriffith$elm_ui$Internal$Model$AsColumn = {$: 'AsColumn'};
var $mdgriffith$elm_ui$Internal$Model$asColumn = $mdgriffith$elm_ui$Internal$Model$AsColumn;
var $mdgriffith$elm_ui$Element$column = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asColumn,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.contentTop + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.contentLeft)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Internal$Model$Fill = function (a) {
	return {$: 'Fill', a: a};
};
var $mdgriffith$elm_ui$Element$fill = $mdgriffith$elm_ui$Internal$Model$Fill(1);
var $mdgriffith$elm_ui$Internal$Model$unstyled = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Unstyled, $elm$core$Basics$always);
var $mdgriffith$elm_ui$Element$html = $mdgriffith$elm_ui$Internal$Model$unstyled;
var $author$project$MyCharts$RemoveChart = function (a) {
	return {$: 'RemoveChart', a: a};
};
var $author$project$MyCharts$removeChartButton = F2(
	function (chartID, msgToMainMsg) {
		return A2(
			$mdgriffith$elm_ui$Element$Input$button,
			_List_fromArray(
				[$mdgriffith$elm_ui$Element$alignLeft]),
			{
				label: A2(
					$mdgriffith$elm_ui$Element$el,
					_List_Nil,
					$mdgriffith$elm_ui$Element$text('Remove Chart')),
				onPress: $elm$core$Maybe$Just(
					msgToMainMsg(
						$author$project$MyCharts$RemoveChart(chartID)))
			});
	});
var $mdgriffith$elm_ui$Internal$Model$AsRow = {$: 'AsRow'};
var $mdgriffith$elm_ui$Internal$Model$asRow = $mdgriffith$elm_ui$Internal$Model$AsRow;
var $mdgriffith$elm_ui$Element$row = F2(
	function (attrs, children) {
		return A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asRow,
			$mdgriffith$elm_ui$Internal$Model$div,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.contentLeft + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.contentCenterY)),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$shrink),
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink),
						attrs))),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var $mdgriffith$elm_ui$Internal$Model$SpacingStyle = F3(
	function (a, b, c) {
		return {$: 'SpacingStyle', a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Internal$Flag$spacing = $mdgriffith$elm_ui$Internal$Flag$flag(3);
var $mdgriffith$elm_ui$Internal$Model$spacingName = F2(
	function (x, y) {
		return 'spacing-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y)));
	});
var $mdgriffith$elm_ui$Element$spacing = function (x) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$spacing,
		A3(
			$mdgriffith$elm_ui$Internal$Model$SpacingStyle,
			A2($mdgriffith$elm_ui$Internal$Model$spacingName, x, x),
			x,
			x));
};
var $terezka$elm_charts$Internal$Svg$Start = {$: 'Start'};
var $terezka$elm_charts$Chart$Attributes$alignLeft = function (config) {
	return _Utils_update(
		config,
		{
			anchor: $elm$core$Maybe$Just($terezka$elm_charts$Internal$Svg$Start)
		});
};
var $gampleman$elm_visualization$Zoom$asRecord = function (_v0) {
	var zoom = _v0.a;
	return {
		scale: zoom.transform.k,
		translate: {x: zoom.transform.x, y: zoom.transform.y}
	};
};
var $terezka$elm_charts$Chart$Attributes$attrs = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{attrs: v});
	});
var $terezka$elm_charts$Internal$Svg$Event = F2(
	function (name, handler) {
		return {handler: handler, name: name};
	});
var $terezka$elm_charts$Internal$Helpers$apply = F2(
	function (funcs, _default) {
		var apply_ = F2(
			function (f, a) {
				return f(a);
			});
		return A3($elm$core$List$foldl, apply_, _default, funcs);
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
var $debois$elm_dom$DOM$offsetHeight = A2($elm$json$Json$Decode$field, 'offsetHeight', $elm$json$Json$Decode$float);
var $debois$elm_dom$DOM$offsetWidth = A2($elm$json$Json$Decode$field, 'offsetWidth', $elm$json$Json$Decode$float);
var $elm$json$Json$Decode$map4 = _Json_map4;
var $debois$elm_dom$DOM$offsetLeft = A2($elm$json$Json$Decode$field, 'offsetLeft', $elm$json$Json$Decode$float);
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $debois$elm_dom$DOM$offsetParent = F2(
	function (x, decoder) {
		return $elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$json$Json$Decode$field,
					'offsetParent',
					$elm$json$Json$Decode$null(x)),
					A2($elm$json$Json$Decode$field, 'offsetParent', decoder)
				]));
	});
var $debois$elm_dom$DOM$offsetTop = A2($elm$json$Json$Decode$field, 'offsetTop', $elm$json$Json$Decode$float);
var $debois$elm_dom$DOM$scrollLeft = A2($elm$json$Json$Decode$field, 'scrollLeft', $elm$json$Json$Decode$float);
var $debois$elm_dom$DOM$scrollTop = A2($elm$json$Json$Decode$field, 'scrollTop', $elm$json$Json$Decode$float);
var $debois$elm_dom$DOM$position = F2(
	function (x, y) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (_v0) {
				var x_ = _v0.a;
				var y_ = _v0.b;
				return A2(
					$debois$elm_dom$DOM$offsetParent,
					_Utils_Tuple2(x_, y_),
					A2($debois$elm_dom$DOM$position, x_, y_));
			},
			A5(
				$elm$json$Json$Decode$map4,
				F4(
					function (scrollLeftP, scrollTopP, offsetLeftP, offsetTopP) {
						return _Utils_Tuple2((x + offsetLeftP) - scrollLeftP, (y + offsetTopP) - scrollTopP);
					}),
				$debois$elm_dom$DOM$scrollLeft,
				$debois$elm_dom$DOM$scrollTop,
				$debois$elm_dom$DOM$offsetLeft,
				$debois$elm_dom$DOM$offsetTop));
	});
var $debois$elm_dom$DOM$boundingClientRect = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (_v0, width, height) {
			var x = _v0.a;
			var y = _v0.b;
			return {height: height, left: x, top: y, width: width};
		}),
	A2($debois$elm_dom$DOM$position, 0, 0),
	$debois$elm_dom$DOM$offsetWidth,
	$debois$elm_dom$DOM$offsetHeight);
var $elm$json$Json$Decode$lazy = function (thunk) {
	return A2(
		$elm$json$Json$Decode$andThen,
		thunk,
		$elm$json$Json$Decode$succeed(_Utils_Tuple0));
};
var $debois$elm_dom$DOM$parentElement = function (decoder) {
	return A2($elm$json$Json$Decode$field, 'parentElement', decoder);
};
function $terezka$elm_charts$Internal$Svg$cyclic$decodePosition() {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$debois$elm_dom$DOM$boundingClientRect,
				$elm$json$Json$Decode$lazy(
				function (_v0) {
					return $debois$elm_dom$DOM$parentElement(
						$terezka$elm_charts$Internal$Svg$cyclic$decodePosition());
				})
			]));
}
try {
	var $terezka$elm_charts$Internal$Svg$decodePosition = $terezka$elm_charts$Internal$Svg$cyclic$decodePosition();
	$terezka$elm_charts$Internal$Svg$cyclic$decodePosition = function () {
		return $terezka$elm_charts$Internal$Svg$decodePosition;
	};
} catch ($) {
	throw 'Some top-level definitions from `Internal.Svg` are causing infinite recursion:\n\n  ┌─────┐\n  │    decodePosition\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.1/bad-recursion to learn how to fix it!';}
var $terezka$elm_charts$Internal$Coordinates$innerWidth = function (plane) {
	return A2($elm$core$Basics$max, 1, (plane.width - plane.margin.left) - plane.margin.right);
};
var $terezka$elm_charts$Internal$Coordinates$range = function (limits) {
	var diff = limits.max - limits.min;
	return (diff > 0) ? diff : 1;
};
var $terezka$elm_charts$Internal$Coordinates$scaleCartesianX = F2(
	function (plane, value) {
		return (value * $terezka$elm_charts$Internal$Coordinates$range(plane.x)) / $terezka$elm_charts$Internal$Coordinates$innerWidth(plane);
	});
var $terezka$elm_charts$Internal$Coordinates$toCartesianX = F2(
	function (plane, value) {
		return A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, value - plane.margin.left) + plane.x.min;
	});
var $terezka$elm_charts$Internal$Coordinates$innerHeight = function (plane) {
	return A2($elm$core$Basics$max, 1, (plane.height - plane.margin.top) - plane.margin.bottom);
};
var $terezka$elm_charts$Internal$Coordinates$scaleCartesianY = F2(
	function (plane, value) {
		return (value * $terezka$elm_charts$Internal$Coordinates$range(plane.y)) / $terezka$elm_charts$Internal$Coordinates$innerHeight(plane);
	});
var $terezka$elm_charts$Internal$Coordinates$toCartesianY = F2(
	function (plane, value) {
		return ($terezka$elm_charts$Internal$Coordinates$range(plane.y) - A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, value - plane.margin.top)) + plane.y.min;
	});
var $terezka$elm_charts$Internal$Svg$fromSvg = F2(
	function (plane, point) {
		return {
			x: A2($terezka$elm_charts$Internal$Coordinates$toCartesianX, plane, point.x),
			y: A2($terezka$elm_charts$Internal$Coordinates$toCartesianY, plane, point.y)
		};
	});
var $debois$elm_dom$DOM$target = function (decoder) {
	return A2($elm$json$Json$Decode$field, 'target', decoder);
};
var $terezka$elm_charts$Internal$Svg$decoder = F2(
	function (plane, toMsg) {
		var handle = F3(
			function (mouseX, mouseY, box) {
				var yPrev = plane.y;
				var xPrev = plane.x;
				var widthPercent = box.width / plane.width;
				var heightPercent = box.height / plane.height;
				var newPlane = _Utils_update(
					plane,
					{
						height: box.height,
						margin: {bottom: plane.margin.bottom * heightPercent, left: plane.margin.left * widthPercent, right: plane.margin.right * widthPercent, top: plane.margin.top * heightPercent},
						width: box.width
					});
				var searched = A2(
					$terezka$elm_charts$Internal$Svg$fromSvg,
					newPlane,
					{x: mouseX - box.left, y: mouseY - box.top});
				return A2(toMsg, newPlane, searched);
			});
		return A4(
			$elm$json$Json$Decode$map3,
			handle,
			A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float),
			$debois$elm_dom$DOM$target($terezka$elm_charts$Internal$Svg$decodePosition));
	});
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$Events$on = $elm$html$Html$Events$on;
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $terezka$elm_charts$Internal$Svg$container = F5(
	function (plane, config, below, chartEls, above) {
		var toEvent = function (event) {
			return A2(
				$elm$svg$Svg$Events$on,
				event.name,
				A2($terezka$elm_charts$Internal$Svg$decoder, plane, event.handler));
		};
		var svgAttrsSize = config.responsive ? _List_fromArray(
			[
				$elm$svg$Svg$Attributes$viewBox(
				'0 0 ' + ($elm$core$String$fromFloat(plane.width) + (' ' + $elm$core$String$fromFloat(plane.height)))),
				A2($elm$html$Html$Attributes$style, 'display', 'block')
			]) : _List_fromArray(
			[
				$elm$svg$Svg$Attributes$width(
				$elm$core$String$fromFloat(plane.width)),
				$elm$svg$Svg$Attributes$height(
				$elm$core$String$fromFloat(plane.height)),
				A2($elm$html$Html$Attributes$style, 'display', 'block')
			]);
		var htmlAttrsSize = config.responsive ? _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'width', '100%'),
				A2($elm$html$Html$Attributes$style, 'height', '100%')
			]) : _List_fromArray(
			[
				A2(
				$elm$html$Html$Attributes$style,
				'width',
				$elm$core$String$fromFloat(plane.width) + 'px'),
				A2(
				$elm$html$Html$Attributes$style,
				'height',
				$elm$core$String$fromFloat(plane.height) + 'px')
			]);
		var htmlAttrsDef = _List_fromArray(
			[
				$elm$html$Html$Attributes$class('elm-charts__container-inner')
			]);
		var htmlAttrs = _Utils_ap(
			config.htmlAttrs,
			_Utils_ap(htmlAttrsDef, htmlAttrsSize));
		var chartPosition = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$x(
				$elm$core$String$fromFloat(plane.margin.left)),
				$elm$svg$Svg$Attributes$y(
				$elm$core$String$fromFloat(plane.margin.top)),
				$elm$svg$Svg$Attributes$width(
				$elm$core$String$fromFloat((plane.width - plane.margin.left) - plane.margin.right)),
				$elm$svg$Svg$Attributes$height(
				$elm$core$String$fromFloat((plane.height - plane.margin.bottom) - plane.margin.top)),
				$elm$svg$Svg$Attributes$fill('transparent')
			]);
		var catcher = A2(
			$elm$svg$Svg$rect,
			_Utils_ap(
				chartPosition,
				A2($elm$core$List$map, toEvent, config.events)),
			_List_Nil);
		var chart = A2(
			$elm$svg$Svg$svg,
			_Utils_ap(svgAttrsSize, config.attrs),
			_Utils_ap(
				chartEls,
				_List_fromArray(
					[catcher])));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('elm-charts__container'),
					A2($elm$html$Html$Attributes$style, 'position', 'relative')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					htmlAttrs,
					_Utils_ap(
						below,
						_Utils_ap(
							_List_fromArray(
								[chart]),
							above)))
				]));
	});
var $terezka$elm_charts$Internal$Coordinates$Position = F4(
	function (x1, x2, y1, y2) {
		return {x1: x1, x2: x2, y1: y1, y2: y2};
	});
var $terezka$elm_charts$Internal$Coordinates$foldPosition = F2(
	function (func, data) {
		var fold = F2(
			function (datum, posM) {
				if (posM.$ === 'Just') {
					var pos = posM.a;
					return $elm$core$Maybe$Just(
						{
							x1: A2(
								$elm$core$Basics$min,
								func(datum).x1,
								pos.x1),
							x2: A2(
								$elm$core$Basics$max,
								func(datum).x2,
								pos.x2),
							y1: A2(
								$elm$core$Basics$min,
								func(datum).y1,
								pos.y1),
							y2: A2(
								$elm$core$Basics$max,
								func(datum).y2,
								pos.y2)
						});
				} else {
					return $elm$core$Maybe$Just(
						func(datum));
				}
			});
		return A2(
			$elm$core$Maybe$withDefault,
			A4($terezka$elm_charts$Internal$Coordinates$Position, 0, 0, 0, 0),
			A3($elm$core$List$foldl, fold, $elm$core$Maybe$Nothing, data));
	});
var $terezka$elm_charts$Chart$Attributes$lowest = F3(
	function (v, edit, b) {
		return _Utils_update(
			b,
			{
				min: A3(edit, v, b.min, b.dataMin)
			});
	});
var $terezka$elm_charts$Chart$Attributes$orLower = F3(
	function (least, real, _v0) {
		return (_Utils_cmp(real, least) > 0) ? least : real;
	});
var $terezka$elm_charts$Chart$definePlane = F2(
	function (config, elements) {
		var toLimit = F2(
			function (min, max) {
				return {dataMax: max, dataMin: min, max: max, min: min};
			});
		var fixSingles = function (bs) {
			return _Utils_eq(bs.min, bs.max) ? _Utils_update(
				bs,
				{max: bs.min + 10}) : bs;
		};
		var collectLimits = F2(
			function (el, acc) {
				switch (el.$) {
					case 'Indexed':
						return acc;
					case 'SeriesElement':
						var lims = el.a;
						return _Utils_ap(acc, lims);
					case 'BarsElement':
						var lims = el.a;
						return _Utils_ap(acc, lims);
					case 'CustomElement':
						return acc;
					case 'AxisElement':
						return acc;
					case 'TicksElement':
						return acc;
					case 'TickElement':
						return acc;
					case 'LabelsElement':
						return acc;
					case 'LabelElement':
						return acc;
					case 'GridElement':
						return acc;
					case 'SubElements':
						return acc;
					case 'ListOfElements':
						var subs = el.a;
						return A3($elm$core$List$foldl, collectLimits, acc, subs);
					case 'SvgElement':
						return acc;
					default:
						return acc;
				}
			});
		var limits_ = function (pos) {
			return function (_v3) {
				var x = _v3.x;
				var y = _v3.y;
				return {
					x: fixSingles(x),
					y: fixSingles(y)
				};
			}(
				{
					x: A2(toLimit, pos.x1, pos.x2),
					y: A2(toLimit, pos.y1, pos.y2)
				});
		}(
			A2(
				$terezka$elm_charts$Internal$Coordinates$foldPosition,
				$elm$core$Basics$identity,
				A3($elm$core$List$foldl, collectLimits, _List_Nil, elements)));
		var calcRange = function () {
			var _v2 = config.range;
			if (!_v2.b) {
				return limits_.x;
			} else {
				var some = _v2;
				return A3(
					$elm$core$List$foldl,
					F2(
						function (f, b) {
							return f(b);
						}),
					limits_.x,
					some);
			}
		}();
		var calcDomain = function () {
			var _v1 = config.domain;
			if (!_v1.b) {
				return A3($terezka$elm_charts$Chart$Attributes$lowest, 0, $terezka$elm_charts$Chart$Attributes$orLower, limits_.y);
			} else {
				var some = _v1;
				return A3(
					$elm$core$List$foldl,
					F2(
						function (f, b) {
							return f(b);
						}),
					limits_.y,
					some);
			}
		}();
		var unpadded = {
			height: A2($elm$core$Basics$max, 1, (config.height - config.padding.bottom) - config.padding.top),
			margin: config.margin,
			width: A2($elm$core$Basics$max, 1, (config.width - config.padding.left) - config.padding.right),
			x: calcRange,
			y: calcDomain
		};
		var scalePadX = $terezka$elm_charts$Internal$Coordinates$scaleCartesianX(unpadded);
		var xMax = calcRange.max + scalePadX(config.padding.right);
		var xMin = calcRange.min - scalePadX(config.padding.left);
		var scalePadY = $terezka$elm_charts$Internal$Coordinates$scaleCartesianY(unpadded);
		var yMax = calcDomain.max + scalePadY(config.padding.top);
		var yMin = calcDomain.min - scalePadY(config.padding.bottom);
		return {
			height: config.height,
			margin: config.margin,
			width: config.width,
			x: _Utils_update(
				calcRange,
				{
					max: A2($elm$core$Basics$max, xMin, xMax),
					min: A2($elm$core$Basics$min, xMin, xMax)
				}),
			y: _Utils_update(
				calcDomain,
				{
					max: A2($elm$core$Basics$max, yMin, yMax),
					min: A2($elm$core$Basics$min, yMin, yMax)
				})
		};
	});
var $terezka$elm_charts$Chart$getItems = F2(
	function (plane, elements) {
		var toItems = F2(
			function (el, acc) {
				switch (el.$) {
					case 'Indexed':
						return acc;
					case 'SeriesElement':
						var items = el.b;
						return _Utils_ap(acc, items);
					case 'BarsElement':
						var items = el.b;
						return _Utils_ap(acc, items);
					case 'CustomElement':
						var item = el.a;
						return _Utils_ap(
							acc,
							_List_fromArray(
								[item]));
					case 'AxisElement':
						var func = el.a;
						return acc;
					case 'TicksElement':
						return acc;
					case 'TickElement':
						return acc;
					case 'LabelsElement':
						return acc;
					case 'LabelElement':
						return acc;
					case 'GridElement':
						return acc;
					case 'SubElements':
						return acc;
					case 'ListOfElements':
						var subs = el.a;
						return A3($elm$core$List$foldl, toItems, acc, subs);
					case 'SvgElement':
						return acc;
					default:
						return acc;
				}
			});
		return A3($elm$core$List$foldl, toItems, _List_Nil, elements);
	});
var $terezka$elm_charts$Chart$getLegends = function (elements) {
	var toLegends = F2(
		function (el, acc) {
			switch (el.$) {
				case 'Indexed':
					return acc;
				case 'SeriesElement':
					var legends_ = el.c;
					return _Utils_ap(acc, legends_);
				case 'BarsElement':
					var legends_ = el.c;
					return _Utils_ap(acc, legends_);
				case 'CustomElement':
					return acc;
				case 'AxisElement':
					return acc;
				case 'TicksElement':
					return acc;
				case 'TickElement':
					return acc;
				case 'LabelsElement':
					return acc;
				case 'LabelElement':
					return acc;
				case 'GridElement':
					return acc;
				case 'SubElements':
					return acc;
				case 'ListOfElements':
					var subs = el.a;
					return A3($elm$core$List$foldl, toLegends, acc, subs);
				case 'SvgElement':
					return acc;
				default:
					return acc;
			}
		});
	return A3($elm$core$List$foldl, toLegends, _List_Nil, elements);
};
var $terezka$elm_charts$Chart$TickValues = F4(
	function (xAxis, yAxis, xs, ys) {
		return {xAxis: xAxis, xs: xs, yAxis: yAxis, ys: ys};
	});
var $terezka$elm_charts$Chart$getTickValues = F3(
	function (plane, items, elements) {
		var toValues = F2(
			function (el, acc) {
				switch (el.$) {
					case 'Indexed':
						return acc;
					case 'SeriesElement':
						return acc;
					case 'BarsElement':
						var func = el.d;
						return A2(func, plane, acc);
					case 'CustomElement':
						var func = el.b;
						return acc;
					case 'AxisElement':
						var func = el.a;
						return A2(func, plane, acc);
					case 'TicksElement':
						var func = el.a;
						return A2(func, plane, acc);
					case 'TickElement':
						var toC = el.a;
						var func = el.b;
						return A3(
							func,
							plane,
							toC(plane),
							acc);
					case 'LabelsElement':
						var toC = el.a;
						var func = el.b;
						return A3(
							func,
							plane,
							toC(plane),
							acc);
					case 'LabelElement':
						var toC = el.a;
						var func = el.b;
						return A3(
							func,
							plane,
							toC(plane),
							acc);
					case 'SubElements':
						var func = el.a;
						return A3(
							$elm$core$List$foldl,
							toValues,
							acc,
							A2(func, plane, items));
					case 'GridElement':
						return acc;
					case 'ListOfElements':
						var subs = el.a;
						return A3($elm$core$List$foldl, toValues, acc, subs);
					case 'SvgElement':
						return acc;
					default:
						return acc;
				}
			});
		return A3(
			$elm$core$List$foldl,
			toValues,
			A4($terezka$elm_charts$Chart$TickValues, _List_Nil, _List_Nil, _List_Nil, _List_Nil),
			elements);
	});
var $terezka$elm_charts$Chart$GridElement = function (a) {
	return {$: 'GridElement', a: a};
};
var $terezka$elm_charts$Internal$Svg$Circle = {$: 'Circle'};
var $terezka$elm_charts$Chart$Attributes$circle = function (config) {
	return _Utils_update(
		config,
		{
			shape: $elm$core$Maybe$Just($terezka$elm_charts$Internal$Svg$Circle)
		});
};
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $terezka$elm_charts$Chart$Attributes$color = F2(
	function (v, config) {
		return (v === '') ? config : _Utils_update(
			config,
			{color: v});
	});
var $terezka$elm_charts$Internal$Helpers$darkGray = 'rgb(200 200 200)';
var $terezka$elm_charts$Chart$Attributes$dashed = F2(
	function (value, config) {
		return _Utils_update(
			config,
			{dashed: value});
	});
var $terezka$elm_charts$Internal$Helpers$pink = '#ea60df';
var $terezka$elm_charts$Internal$Svg$defaultDot = {border: '', borderWidth: 0, color: $terezka$elm_charts$Internal$Helpers$pink, highlight: 0, highlightColor: '', highlightWidth: 5, opacity: 1, shape: $elm$core$Maybe$Nothing, size: 6};
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$Attributes$fillOpacity = _VirtualDom_attribute('fill-opacity');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $terezka$elm_charts$Internal$Svg$isWithinPlane = F3(
	function (plane, x, y) {
		return _Utils_eq(
			A3($elm$core$Basics$clamp, plane.x.min, plane.x.max, x),
			x) && _Utils_eq(
			A3($elm$core$Basics$clamp, plane.y.min, plane.y.max, y),
			y);
	});
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$core$Basics$pi = _Basics_pi;
var $terezka$elm_charts$Internal$Svg$plusPath = F4(
	function (area_, off, x_, y_) {
		var side = $elm$core$Basics$sqrt(area_ / 4) + off;
		var r6 = side / 2;
		var r3 = side;
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					'M' + ($elm$core$String$fromFloat(x_ - r6) + (' ' + $elm$core$String$fromFloat(((y_ - r3) - r6) + off))),
					'v' + $elm$core$String$fromFloat(r3 - off),
					'h' + $elm$core$String$fromFloat((-r3) + off),
					'v' + $elm$core$String$fromFloat(r3),
					'h' + $elm$core$String$fromFloat(r3 - off),
					'v' + $elm$core$String$fromFloat(r3 - off),
					'h' + $elm$core$String$fromFloat(r3),
					'v' + $elm$core$String$fromFloat((-r3) + off),
					'h' + $elm$core$String$fromFloat(r3 - off),
					'v' + $elm$core$String$fromFloat(-r3),
					'h' + $elm$core$String$fromFloat((-r3) + off),
					'v' + $elm$core$String$fromFloat((-r3) + off),
					'h' + $elm$core$String$fromFloat(-r3),
					'v' + $elm$core$String$fromFloat(r3 - off)
				]));
	});
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeOpacity = _VirtualDom_attribute('stroke-opacity');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $terezka$elm_charts$Internal$Coordinates$scaleSVGX = F2(
	function (plane, value) {
		return (value * $terezka$elm_charts$Internal$Coordinates$innerWidth(plane)) / $terezka$elm_charts$Internal$Coordinates$range(plane.x);
	});
var $terezka$elm_charts$Internal$Coordinates$toSVGX = F2(
	function (plane, value) {
		return A2($terezka$elm_charts$Internal$Coordinates$scaleSVGX, plane, value - plane.x.min) + plane.margin.left;
	});
var $terezka$elm_charts$Internal$Coordinates$scaleSVGY = F2(
	function (plane, value) {
		return (value * $terezka$elm_charts$Internal$Coordinates$innerHeight(plane)) / $terezka$elm_charts$Internal$Coordinates$range(plane.y);
	});
var $terezka$elm_charts$Internal$Coordinates$toSVGY = F2(
	function (plane, value) {
		return A2($terezka$elm_charts$Internal$Coordinates$scaleSVGY, plane, plane.y.max - value) + plane.margin.top;
	});
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $elm$core$Basics$degrees = function (angleInDegrees) {
	return (angleInDegrees * $elm$core$Basics$pi) / 180;
};
var $elm$core$Basics$tan = _Basics_tan;
var $terezka$elm_charts$Internal$Svg$trianglePath = F4(
	function (area_, off, x_, y_) {
		var side = $elm$core$Basics$sqrt(
			(area_ * 4) / $elm$core$Basics$sqrt(3)) + (off * $elm$core$Basics$sqrt(3));
		var height = ($elm$core$Basics$sqrt(3) * side) / 2;
		var fromMiddle = height - (($elm$core$Basics$tan(
			$elm$core$Basics$degrees(30)) * side) / 2);
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					'M' + ($elm$core$String$fromFloat(x_) + (' ' + $elm$core$String$fromFloat(y_ - fromMiddle))),
					'l' + ($elm$core$String$fromFloat((-side) / 2) + (' ' + $elm$core$String$fromFloat(height))),
					'h' + $elm$core$String$fromFloat(side),
					'z'
				]));
	});
var $terezka$elm_charts$Internal$Svg$dot = F5(
	function (plane, toX, toY, config, datum_) {
		var y_ = A2(
			$terezka$elm_charts$Internal$Coordinates$toSVGY,
			plane,
			toY(datum_));
		var x_ = A2(
			$terezka$elm_charts$Internal$Coordinates$toSVGX,
			plane,
			toX(datum_));
		var styleAttrs = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$stroke(
				(config.border === '') ? config.color : config.border),
				$elm$svg$Svg$Attributes$strokeWidth(
				$elm$core$String$fromFloat(config.borderWidth)),
				$elm$svg$Svg$Attributes$fillOpacity(
				$elm$core$String$fromFloat(config.opacity)),
				$elm$svg$Svg$Attributes$fill(config.color),
				$elm$svg$Svg$Attributes$class('elm-charts__dot')
			]);
		var highlightColor = (config.highlightColor === '') ? config.color : config.highlightColor;
		var highlightAttrs = _List_fromArray(
			[
				$elm$svg$Svg$Attributes$stroke(highlightColor),
				$elm$svg$Svg$Attributes$strokeWidth(
				$elm$core$String$fromFloat(config.highlightWidth)),
				$elm$svg$Svg$Attributes$strokeOpacity(
				$elm$core$String$fromFloat(config.highlight)),
				$elm$svg$Svg$Attributes$fill('transparent'),
				$elm$svg$Svg$Attributes$class('elm-charts__dot-highlight')
			]);
		var view = F3(
			function (toEl, highlightOff, toAttrs) {
				return (config.highlight > 0) ? A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('elm-charts__dot-container')
						]),
					_List_fromArray(
						[
							A2(
							toEl,
							_Utils_ap(
								toAttrs(highlightOff),
								highlightAttrs),
							_List_Nil),
							A2(
							toEl,
							_Utils_ap(
								toAttrs(0),
								styleAttrs),
							_List_Nil)
						])) : A2(
					toEl,
					_Utils_ap(
						toAttrs(0),
						styleAttrs),
					_List_Nil);
			});
		var area_ = (2 * $elm$core$Basics$pi) * config.size;
		if (!A3(
			$terezka$elm_charts$Internal$Svg$isWithinPlane,
			plane,
			toX(datum_),
			toY(datum_))) {
			return $elm$svg$Svg$text('');
		} else {
			var _v0 = config.shape;
			if (_v0.$ === 'Nothing') {
				return $elm$svg$Svg$text('');
			} else {
				switch (_v0.a.$) {
					case 'Circle':
						var _v1 = _v0.a;
						return A3(
							view,
							$elm$svg$Svg$circle,
							config.highlightWidth / 2,
							function (off) {
								var radius = $elm$core$Basics$sqrt(area_ / $elm$core$Basics$pi);
								return _List_fromArray(
									[
										$elm$svg$Svg$Attributes$cx(
										$elm$core$String$fromFloat(x_)),
										$elm$svg$Svg$Attributes$cy(
										$elm$core$String$fromFloat(y_)),
										$elm$svg$Svg$Attributes$r(
										$elm$core$String$fromFloat(radius + off))
									]);
							});
					case 'Triangle':
						var _v2 = _v0.a;
						return A3(
							view,
							$elm$svg$Svg$path,
							config.highlightWidth,
							function (off) {
								return _List_fromArray(
									[
										$elm$svg$Svg$Attributes$d(
										A4($terezka$elm_charts$Internal$Svg$trianglePath, area_, off, x_, y_))
									]);
							});
					case 'Square':
						var _v3 = _v0.a;
						return A3(
							view,
							$elm$svg$Svg$rect,
							config.highlightWidth,
							function (off) {
								var side = $elm$core$Basics$sqrt(area_);
								var sideOff = side + off;
								return _List_fromArray(
									[
										$elm$svg$Svg$Attributes$x(
										$elm$core$String$fromFloat(x_ - (sideOff / 2))),
										$elm$svg$Svg$Attributes$y(
										$elm$core$String$fromFloat(y_ - (sideOff / 2))),
										$elm$svg$Svg$Attributes$width(
										$elm$core$String$fromFloat(sideOff)),
										$elm$svg$Svg$Attributes$height(
										$elm$core$String$fromFloat(sideOff))
									]);
							});
					case 'Diamond':
						var _v4 = _v0.a;
						return A3(
							view,
							$elm$svg$Svg$rect,
							config.highlightWidth,
							function (off) {
								var side = $elm$core$Basics$sqrt(area_);
								var sideOff = side + off;
								return _List_fromArray(
									[
										$elm$svg$Svg$Attributes$x(
										$elm$core$String$fromFloat(x_ - (sideOff / 2))),
										$elm$svg$Svg$Attributes$y(
										$elm$core$String$fromFloat(y_ - (sideOff / 2))),
										$elm$svg$Svg$Attributes$width(
										$elm$core$String$fromFloat(sideOff)),
										$elm$svg$Svg$Attributes$height(
										$elm$core$String$fromFloat(sideOff)),
										$elm$svg$Svg$Attributes$transform(
										'rotate(45 ' + ($elm$core$String$fromFloat(x_) + (' ' + ($elm$core$String$fromFloat(y_) + ')'))))
									]);
							});
					case 'Cross':
						var _v5 = _v0.a;
						return A3(
							view,
							$elm$svg$Svg$path,
							config.highlightWidth,
							function (off) {
								return _List_fromArray(
									[
										$elm$svg$Svg$Attributes$d(
										A4($terezka$elm_charts$Internal$Svg$plusPath, area_, off, x_, y_)),
										$elm$svg$Svg$Attributes$transform(
										'rotate(45 ' + ($elm$core$String$fromFloat(x_) + (' ' + ($elm$core$String$fromFloat(y_) + ')'))))
									]);
							});
					default:
						var _v6 = _v0.a;
						return A3(
							view,
							$elm$svg$Svg$path,
							config.highlightWidth,
							function (off) {
								return _List_fromArray(
									[
										$elm$svg$Svg$Attributes$d(
										A4($terezka$elm_charts$Internal$Svg$plusPath, area_, off, x_, y_))
									]);
							});
				}
			}
		}
	});
var $terezka$elm_charts$Chart$Svg$dot = F4(
	function (plane, toX, toY, edits) {
		return A4(
			$terezka$elm_charts$Internal$Svg$dot,
			plane,
			toX,
			toY,
			A2($terezka$elm_charts$Internal$Helpers$apply, edits, $terezka$elm_charts$Internal$Svg$defaultDot));
	});
var $terezka$elm_charts$Internal$Helpers$gray = '#EFF2FA';
var $terezka$elm_charts$Internal$Svg$defaultLine = {attrs: _List_Nil, _break: false, color: 'rgb(210, 210, 210)', dashed: _List_Nil, flip: false, opacity: 1, tickDirection: -90, tickLength: 0, width: 1, x1: $elm$core$Maybe$Nothing, x2: $elm$core$Maybe$Nothing, x2Svg: $elm$core$Maybe$Nothing, xOff: 0, y1: $elm$core$Maybe$Nothing, y2: $elm$core$Maybe$Nothing, y2Svg: $elm$core$Maybe$Nothing, yOff: 0};
var $terezka$elm_charts$Internal$Commands$Line = F2(
	function (a, b) {
		return {$: 'Line', a: a, b: b};
	});
var $terezka$elm_charts$Internal$Commands$Move = F2(
	function (a, b) {
		return {$: 'Move', a: a, b: b};
	});
var $elm$core$Basics$cos = _Basics_cos;
var $terezka$elm_charts$Internal$Commands$joinCommands = function (commands) {
	return A2($elm$core$String$join, ' ', commands);
};
var $terezka$elm_charts$Internal$Commands$stringBoolInt = function (bool) {
	return bool ? '1' : '0';
};
var $terezka$elm_charts$Internal$Commands$stringPoint = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return $elm$core$String$fromFloat(x) + (' ' + $elm$core$String$fromFloat(y));
};
var $terezka$elm_charts$Internal$Commands$stringPoints = function (points) {
	return A2(
		$elm$core$String$join,
		',',
		A2($elm$core$List$map, $terezka$elm_charts$Internal$Commands$stringPoint, points));
};
var $terezka$elm_charts$Internal$Commands$stringCommand = function (command) {
	switch (command.$) {
		case 'Move':
			var x = command.a;
			var y = command.b;
			return 'M' + $terezka$elm_charts$Internal$Commands$stringPoint(
				_Utils_Tuple2(x, y));
		case 'Line':
			var x = command.a;
			var y = command.b;
			return 'L' + $terezka$elm_charts$Internal$Commands$stringPoint(
				_Utils_Tuple2(x, y));
		case 'CubicBeziers':
			var cx1 = command.a;
			var cy1 = command.b;
			var cx2 = command.c;
			var cy2 = command.d;
			var x = command.e;
			var y = command.f;
			return 'C' + $terezka$elm_charts$Internal$Commands$stringPoints(
				_List_fromArray(
					[
						_Utils_Tuple2(cx1, cy1),
						_Utils_Tuple2(cx2, cy2),
						_Utils_Tuple2(x, y)
					]));
		case 'CubicBeziersShort':
			var cx1 = command.a;
			var cy1 = command.b;
			var x = command.c;
			var y = command.d;
			return 'Q' + $terezka$elm_charts$Internal$Commands$stringPoints(
				_List_fromArray(
					[
						_Utils_Tuple2(cx1, cy1),
						_Utils_Tuple2(x, y)
					]));
		case 'QuadraticBeziers':
			var cx1 = command.a;
			var cy1 = command.b;
			var x = command.c;
			var y = command.d;
			return 'Q' + $terezka$elm_charts$Internal$Commands$stringPoints(
				_List_fromArray(
					[
						_Utils_Tuple2(cx1, cy1),
						_Utils_Tuple2(x, y)
					]));
		case 'QuadraticBeziersShort':
			var x = command.a;
			var y = command.b;
			return 'T' + $terezka$elm_charts$Internal$Commands$stringPoint(
				_Utils_Tuple2(x, y));
		case 'Arc':
			var rx = command.a;
			var ry = command.b;
			var xAxisRotation = command.c;
			var largeArcFlag = command.d;
			var sweepFlag = command.e;
			var x = command.f;
			var y = command.g;
			return 'A ' + $terezka$elm_charts$Internal$Commands$joinCommands(
				_List_fromArray(
					[
						$terezka$elm_charts$Internal$Commands$stringPoint(
						_Utils_Tuple2(rx, ry)),
						$elm$core$String$fromInt(xAxisRotation),
						$terezka$elm_charts$Internal$Commands$stringBoolInt(largeArcFlag),
						$terezka$elm_charts$Internal$Commands$stringBoolInt(sweepFlag),
						$terezka$elm_charts$Internal$Commands$stringPoint(
						_Utils_Tuple2(x, y))
					]));
		default:
			return 'Z';
	}
};
var $terezka$elm_charts$Internal$Commands$Arc = F7(
	function (a, b, c, d, e, f, g) {
		return {$: 'Arc', a: a, b: b, c: c, d: d, e: e, f: f, g: g};
	});
var $terezka$elm_charts$Internal$Commands$Close = {$: 'Close'};
var $terezka$elm_charts$Internal$Commands$CubicBeziers = F6(
	function (a, b, c, d, e, f) {
		return {$: 'CubicBeziers', a: a, b: b, c: c, d: d, e: e, f: f};
	});
var $terezka$elm_charts$Internal$Commands$CubicBeziersShort = F4(
	function (a, b, c, d) {
		return {$: 'CubicBeziersShort', a: a, b: b, c: c, d: d};
	});
var $terezka$elm_charts$Internal$Commands$QuadraticBeziers = F4(
	function (a, b, c, d) {
		return {$: 'QuadraticBeziers', a: a, b: b, c: c, d: d};
	});
var $terezka$elm_charts$Internal$Commands$QuadraticBeziersShort = F2(
	function (a, b) {
		return {$: 'QuadraticBeziersShort', a: a, b: b};
	});
var $terezka$elm_charts$Internal$Commands$translate = F2(
	function (plane, command) {
		switch (command.$) {
			case 'Move':
				var x = command.a;
				var y = command.b;
				return A2(
					$terezka$elm_charts$Internal$Commands$Move,
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, x),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, y));
			case 'Line':
				var x = command.a;
				var y = command.b;
				return A2(
					$terezka$elm_charts$Internal$Commands$Line,
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, x),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, y));
			case 'CubicBeziers':
				var cx1 = command.a;
				var cy1 = command.b;
				var cx2 = command.c;
				var cy2 = command.d;
				var x = command.e;
				var y = command.f;
				return A6(
					$terezka$elm_charts$Internal$Commands$CubicBeziers,
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, cx1),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, cy1),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, cx2),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, cy2),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, x),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, y));
			case 'CubicBeziersShort':
				var cx1 = command.a;
				var cy1 = command.b;
				var x = command.c;
				var y = command.d;
				return A4(
					$terezka$elm_charts$Internal$Commands$CubicBeziersShort,
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, cx1),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, cy1),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, x),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, y));
			case 'QuadraticBeziers':
				var cx1 = command.a;
				var cy1 = command.b;
				var x = command.c;
				var y = command.d;
				return A4(
					$terezka$elm_charts$Internal$Commands$QuadraticBeziers,
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, cx1),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, cy1),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, x),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, y));
			case 'QuadraticBeziersShort':
				var x = command.a;
				var y = command.b;
				return A2(
					$terezka$elm_charts$Internal$Commands$QuadraticBeziersShort,
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, x),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, y));
			case 'Arc':
				var rx = command.a;
				var ry = command.b;
				var xAxisRotation = command.c;
				var largeArcFlag = command.d;
				var sweepFlag = command.e;
				var x = command.f;
				var y = command.g;
				return A7(
					$terezka$elm_charts$Internal$Commands$Arc,
					rx,
					ry,
					xAxisRotation,
					largeArcFlag,
					sweepFlag,
					A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, x),
					A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, y));
			default:
				return $terezka$elm_charts$Internal$Commands$Close;
		}
	});
var $terezka$elm_charts$Internal$Commands$description = F2(
	function (plane, commands) {
		return $terezka$elm_charts$Internal$Commands$joinCommands(
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeR,
					$terezka$elm_charts$Internal$Commands$translate(plane),
					$terezka$elm_charts$Internal$Commands$stringCommand),
				commands));
	});
var $terezka$elm_charts$Internal$Svg$lengthInCartesianX = $terezka$elm_charts$Internal$Coordinates$scaleCartesianX;
var $terezka$elm_charts$Internal$Svg$lengthInCartesianY = $terezka$elm_charts$Internal$Coordinates$scaleCartesianY;
var $elm$core$Basics$sin = _Basics_sin;
var $elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var $elm$virtual_dom$VirtualDom$mapAttribute = _VirtualDom_mapAttribute;
var $elm$html$Html$Attributes$map = $elm$virtual_dom$VirtualDom$mapAttribute;
var $terezka$elm_charts$Internal$Svg$withAttrs = F3(
	function (attrs, toEl, defaultAttrs) {
		return toEl(
			_Utils_ap(
				defaultAttrs,
				A2(
					$elm$core$List$map,
					$elm$html$Html$Attributes$map($elm$core$Basics$never),
					attrs)));
	});
var $terezka$elm_charts$Internal$Svg$line = F2(
	function (plane, config) {
		var angle = $elm$core$Basics$degrees(config.tickDirection);
		var _v0 = function () {
			var _v3 = _Utils_Tuple3(
				_Utils_Tuple2(config.x1, config.x2),
				_Utils_Tuple2(config.y1, config.y2),
				_Utils_Tuple2(config.x2Svg, config.y2Svg));
			if (_v3.a.a.$ === 'Just') {
				if (_v3.a.b.$ === 'Just') {
					if (_v3.b.a.$ === 'Nothing') {
						if (_v3.b.b.$ === 'Nothing') {
							var _v4 = _v3.a;
							var a = _v4.a.a;
							var b = _v4.b.a;
							var _v5 = _v3.b;
							var _v6 = _v5.a;
							var _v7 = _v5.b;
							return _Utils_Tuple2(
								_Utils_Tuple2(a, b),
								_Utils_Tuple2(plane.y.min, plane.y.min));
						} else {
							var _v38 = _v3.a;
							var a = _v38.a.a;
							var b = _v38.b.a;
							var _v39 = _v3.b;
							var _v40 = _v39.a;
							var c = _v39.b.a;
							return _Utils_Tuple2(
								_Utils_Tuple2(a, b),
								_Utils_Tuple2(c, c));
						}
					} else {
						if (_v3.b.b.$ === 'Nothing') {
							var _v41 = _v3.a;
							var a = _v41.a.a;
							var b = _v41.b.a;
							var _v42 = _v3.b;
							var c = _v42.a.a;
							var _v43 = _v42.b;
							return _Utils_Tuple2(
								_Utils_Tuple2(a, b),
								_Utils_Tuple2(c, c));
						} else {
							return _Utils_Tuple2(
								_Utils_Tuple2(
									A2($elm$core$Maybe$withDefault, plane.x.min, config.x1),
									A2($elm$core$Maybe$withDefault, plane.x.max, config.x2)),
								_Utils_Tuple2(
									A2($elm$core$Maybe$withDefault, plane.y.min, config.y1),
									A2($elm$core$Maybe$withDefault, plane.y.max, config.y2)));
						}
					}
				} else {
					if (_v3.b.a.$ === 'Nothing') {
						if (_v3.b.b.$ === 'Nothing') {
							var _v8 = _v3.a;
							var a = _v8.a.a;
							var _v9 = _v8.b;
							var _v10 = _v3.b;
							var _v11 = _v10.a;
							var _v12 = _v10.b;
							return _Utils_Tuple2(
								_Utils_Tuple2(a, a),
								_Utils_Tuple2(plane.y.min, plane.y.max));
						} else {
							if (_v3.c.a.$ === 'Just') {
								if (_v3.c.b.$ === 'Just') {
									var _v51 = _v3.a;
									var a = _v51.a.a;
									var _v52 = _v51.b;
									var _v53 = _v3.b;
									var _v54 = _v53.a;
									var b = _v53.b.a;
									var _v55 = _v3.c;
									var xOff = _v55.a.a;
									var yOff = _v55.b.a;
									return _Utils_Tuple2(
										_Utils_Tuple2(
											a,
											a + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, xOff)),
										_Utils_Tuple2(
											b,
											b + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, yOff)));
								} else {
									var _v56 = _v3.a;
									var a = _v56.a.a;
									var _v57 = _v56.b;
									var _v58 = _v3.b;
									var _v59 = _v58.a;
									var b = _v58.b.a;
									var _v60 = _v3.c;
									var xOff = _v60.a.a;
									var _v61 = _v60.b;
									return _Utils_Tuple2(
										_Utils_Tuple2(
											a,
											a + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, xOff)),
										_Utils_Tuple2(b, b));
								}
							} else {
								if (_v3.c.b.$ === 'Nothing') {
									var _v44 = _v3.a;
									var a = _v44.a.a;
									var _v45 = _v44.b;
									var _v46 = _v3.b;
									var _v47 = _v46.a;
									var b = _v46.b.a;
									var _v48 = _v3.c;
									var _v49 = _v48.a;
									var _v50 = _v48.b;
									return _Utils_Tuple2(
										_Utils_Tuple2(a, plane.x.max),
										_Utils_Tuple2(b, b));
								} else {
									var _v62 = _v3.a;
									var a = _v62.a.a;
									var _v63 = _v62.b;
									var _v64 = _v3.b;
									var _v65 = _v64.a;
									var b = _v64.b.a;
									var _v66 = _v3.c;
									var _v67 = _v66.a;
									var yOff = _v66.b.a;
									return _Utils_Tuple2(
										_Utils_Tuple2(a, a),
										_Utils_Tuple2(
											b,
											b + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, yOff)));
								}
							}
						}
					} else {
						if (_v3.b.b.$ === 'Just') {
							var _v35 = _v3.a;
							var c = _v35.a.a;
							var _v36 = _v35.b;
							var _v37 = _v3.b;
							var a = _v37.a.a;
							var b = _v37.b.a;
							return _Utils_Tuple2(
								_Utils_Tuple2(c, c),
								_Utils_Tuple2(a, b));
						} else {
							if (_v3.c.a.$ === 'Just') {
								if (_v3.c.b.$ === 'Just') {
									var _v75 = _v3.a;
									var a = _v75.a.a;
									var _v76 = _v75.b;
									var _v77 = _v3.b;
									var b = _v77.a.a;
									var _v78 = _v77.b;
									var _v79 = _v3.c;
									var xOff = _v79.a.a;
									var yOff = _v79.b.a;
									return _Utils_Tuple2(
										_Utils_Tuple2(
											a,
											a + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, xOff)),
										_Utils_Tuple2(
											b,
											b + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, yOff)));
								} else {
									var _v80 = _v3.a;
									var a = _v80.a.a;
									var _v81 = _v80.b;
									var _v82 = _v3.b;
									var b = _v82.a.a;
									var _v83 = _v82.b;
									var _v84 = _v3.c;
									var xOff = _v84.a.a;
									var _v85 = _v84.b;
									return _Utils_Tuple2(
										_Utils_Tuple2(
											a,
											a + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, xOff)),
										_Utils_Tuple2(b, b));
								}
							} else {
								if (_v3.c.b.$ === 'Nothing') {
									var _v68 = _v3.a;
									var a = _v68.a.a;
									var _v69 = _v68.b;
									var _v70 = _v3.b;
									var b = _v70.a.a;
									var _v71 = _v70.b;
									var _v72 = _v3.c;
									var _v73 = _v72.a;
									var _v74 = _v72.b;
									return _Utils_Tuple2(
										_Utils_Tuple2(a, plane.x.max),
										_Utils_Tuple2(b, b));
								} else {
									var _v86 = _v3.a;
									var a = _v86.a.a;
									var _v87 = _v86.b;
									var _v88 = _v3.b;
									var b = _v88.a.a;
									var _v89 = _v88.b;
									var _v90 = _v3.c;
									var _v91 = _v90.a;
									var yOff = _v90.b.a;
									return _Utils_Tuple2(
										_Utils_Tuple2(a, a),
										_Utils_Tuple2(
											b,
											b + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, yOff)));
								}
							}
						}
					}
				}
			} else {
				if (_v3.a.b.$ === 'Just') {
					if (_v3.b.a.$ === 'Nothing') {
						if (_v3.b.b.$ === 'Nothing') {
							var _v13 = _v3.a;
							var _v14 = _v13.a;
							var b = _v13.b.a;
							var _v15 = _v3.b;
							var _v16 = _v15.a;
							var _v17 = _v15.b;
							return _Utils_Tuple2(
								_Utils_Tuple2(b, b),
								_Utils_Tuple2(plane.y.min, plane.y.max));
						} else {
							if (_v3.c.a.$ === 'Just') {
								if (_v3.c.b.$ === 'Just') {
									var _v99 = _v3.a;
									var _v100 = _v99.a;
									var a = _v99.b.a;
									var _v101 = _v3.b;
									var _v102 = _v101.a;
									var b = _v101.b.a;
									var _v103 = _v3.c;
									var xOff = _v103.a.a;
									var yOff = _v103.b.a;
									return _Utils_Tuple2(
										_Utils_Tuple2(
											a,
											a + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, xOff)),
										_Utils_Tuple2(
											b,
											b + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, yOff)));
								} else {
									var _v104 = _v3.a;
									var _v105 = _v104.a;
									var a = _v104.b.a;
									var _v106 = _v3.b;
									var _v107 = _v106.a;
									var b = _v106.b.a;
									var _v108 = _v3.c;
									var xOff = _v108.a.a;
									var _v109 = _v108.b;
									return _Utils_Tuple2(
										_Utils_Tuple2(
											a,
											a + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, xOff)),
										_Utils_Tuple2(b, b));
								}
							} else {
								if (_v3.c.b.$ === 'Nothing') {
									var _v92 = _v3.a;
									var _v93 = _v92.a;
									var a = _v92.b.a;
									var _v94 = _v3.b;
									var _v95 = _v94.a;
									var b = _v94.b.a;
									var _v96 = _v3.c;
									var _v97 = _v96.a;
									var _v98 = _v96.b;
									return _Utils_Tuple2(
										_Utils_Tuple2(a, plane.x.max),
										_Utils_Tuple2(b, b));
								} else {
									var _v110 = _v3.a;
									var _v111 = _v110.a;
									var a = _v110.b.a;
									var _v112 = _v3.b;
									var _v113 = _v112.a;
									var b = _v112.b.a;
									var _v114 = _v3.c;
									var _v115 = _v114.a;
									var yOff = _v114.b.a;
									return _Utils_Tuple2(
										_Utils_Tuple2(a, a),
										_Utils_Tuple2(
											b,
											b + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, yOff)));
								}
							}
						}
					} else {
						if (_v3.b.b.$ === 'Just') {
							var _v32 = _v3.a;
							var _v33 = _v32.a;
							var c = _v32.b.a;
							var _v34 = _v3.b;
							var a = _v34.a.a;
							var b = _v34.b.a;
							return _Utils_Tuple2(
								_Utils_Tuple2(c, c),
								_Utils_Tuple2(a, b));
						} else {
							if (_v3.c.a.$ === 'Just') {
								if (_v3.c.b.$ === 'Just') {
									var _v123 = _v3.a;
									var _v124 = _v123.a;
									var a = _v123.b.a;
									var _v125 = _v3.b;
									var b = _v125.a.a;
									var _v126 = _v125.b;
									var _v127 = _v3.c;
									var xOff = _v127.a.a;
									var yOff = _v127.b.a;
									return _Utils_Tuple2(
										_Utils_Tuple2(
											a,
											a + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, xOff)),
										_Utils_Tuple2(
											b,
											b + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, yOff)));
								} else {
									var _v128 = _v3.a;
									var _v129 = _v128.a;
									var a = _v128.b.a;
									var _v130 = _v3.b;
									var b = _v130.a.a;
									var _v131 = _v130.b;
									var _v132 = _v3.c;
									var xOff = _v132.a.a;
									var _v133 = _v132.b;
									return _Utils_Tuple2(
										_Utils_Tuple2(
											a,
											a + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, xOff)),
										_Utils_Tuple2(b, b));
								}
							} else {
								if (_v3.c.b.$ === 'Nothing') {
									var _v116 = _v3.a;
									var _v117 = _v116.a;
									var a = _v116.b.a;
									var _v118 = _v3.b;
									var b = _v118.a.a;
									var _v119 = _v118.b;
									var _v120 = _v3.c;
									var _v121 = _v120.a;
									var _v122 = _v120.b;
									return _Utils_Tuple2(
										_Utils_Tuple2(a, plane.x.max),
										_Utils_Tuple2(b, b));
								} else {
									var _v134 = _v3.a;
									var _v135 = _v134.a;
									var a = _v134.b.a;
									var _v136 = _v3.b;
									var b = _v136.a.a;
									var _v137 = _v136.b;
									var _v138 = _v3.c;
									var _v139 = _v138.a;
									var yOff = _v138.b.a;
									return _Utils_Tuple2(
										_Utils_Tuple2(a, a),
										_Utils_Tuple2(
											b,
											b + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, yOff)));
								}
							}
						}
					}
				} else {
					if (_v3.b.a.$ === 'Just') {
						if (_v3.b.b.$ === 'Just') {
							var _v18 = _v3.a;
							var _v19 = _v18.a;
							var _v20 = _v18.b;
							var _v21 = _v3.b;
							var a = _v21.a.a;
							var b = _v21.b.a;
							return _Utils_Tuple2(
								_Utils_Tuple2(plane.x.min, plane.x.min),
								_Utils_Tuple2(a, b));
						} else {
							var _v22 = _v3.a;
							var _v23 = _v22.a;
							var _v24 = _v22.b;
							var _v25 = _v3.b;
							var a = _v25.a.a;
							var _v26 = _v25.b;
							return _Utils_Tuple2(
								_Utils_Tuple2(plane.x.min, plane.x.max),
								_Utils_Tuple2(a, a));
						}
					} else {
						if (_v3.b.b.$ === 'Just') {
							var _v27 = _v3.a;
							var _v28 = _v27.a;
							var _v29 = _v27.b;
							var _v30 = _v3.b;
							var _v31 = _v30.a;
							var b = _v30.b.a;
							return _Utils_Tuple2(
								_Utils_Tuple2(plane.x.min, plane.x.max),
								_Utils_Tuple2(b, b));
						} else {
							var _v140 = _v3.a;
							var _v141 = _v140.a;
							var _v142 = _v140.b;
							var _v143 = _v3.b;
							var _v144 = _v143.a;
							var _v145 = _v143.b;
							return _Utils_Tuple2(
								_Utils_Tuple2(plane.x.min, plane.x.max),
								_Utils_Tuple2(plane.y.min, plane.y.max));
						}
					}
				}
			}
		}();
		var _v1 = _v0.a;
		var x1 = _v1.a;
		var x2 = _v1.b;
		var _v2 = _v0.b;
		var y1 = _v2.a;
		var y2 = _v2.b;
		var x1_ = x1 + A2($terezka$elm_charts$Internal$Svg$lengthInCartesianX, plane, config.xOff);
		var x2_ = x2 + A2($terezka$elm_charts$Internal$Svg$lengthInCartesianX, plane, config.xOff);
		var y1_ = y1 - A2($terezka$elm_charts$Internal$Svg$lengthInCartesianY, plane, config.yOff);
		var y2_ = y2 - A2($terezka$elm_charts$Internal$Svg$lengthInCartesianY, plane, config.yOff);
		var _v146 = (config.tickLength > 0) ? _Utils_Tuple2(
			A2(
				$terezka$elm_charts$Internal$Svg$lengthInCartesianX,
				plane,
				$elm$core$Basics$cos(angle) * config.tickLength),
			A2(
				$terezka$elm_charts$Internal$Svg$lengthInCartesianY,
				plane,
				$elm$core$Basics$sin(angle) * config.tickLength)) : _Utils_Tuple2(0, 0);
		var tickOffsetX = _v146.a;
		var tickOffsetY = _v146.b;
		var cmds = config.flip ? _Utils_ap(
			(config.tickLength > 0) ? _List_fromArray(
				[
					A2($terezka$elm_charts$Internal$Commands$Move, x2_ + tickOffsetX, y2_ + tickOffsetY),
					A2($terezka$elm_charts$Internal$Commands$Line, x2_, y2_)
				]) : _List_fromArray(
				[
					A2($terezka$elm_charts$Internal$Commands$Move, x2_, y2_)
				]),
			_Utils_ap(
				config._break ? _List_fromArray(
					[
						A2($terezka$elm_charts$Internal$Commands$Line, x2_, y1_),
						A2($terezka$elm_charts$Internal$Commands$Line, x1_, y1_)
					]) : _List_fromArray(
					[
						A2($terezka$elm_charts$Internal$Commands$Line, x1_, y1_)
					]),
				(config.tickLength > 0) ? _List_fromArray(
					[
						A2($terezka$elm_charts$Internal$Commands$Line, x1_ + tickOffsetX, y1_ + tickOffsetY)
					]) : _List_Nil)) : _Utils_ap(
			(config.tickLength > 0) ? _List_fromArray(
				[
					A2($terezka$elm_charts$Internal$Commands$Move, x1_ + tickOffsetX, y1_ + tickOffsetY),
					A2($terezka$elm_charts$Internal$Commands$Line, x1_, y1_)
				]) : _List_fromArray(
				[
					A2($terezka$elm_charts$Internal$Commands$Move, x1_, y1_)
				]),
			_Utils_ap(
				config._break ? _List_fromArray(
					[
						A2($terezka$elm_charts$Internal$Commands$Line, x1_, y2_),
						A2($terezka$elm_charts$Internal$Commands$Line, x2_, y2_)
					]) : _List_fromArray(
					[
						A2($terezka$elm_charts$Internal$Commands$Line, x2_, y2_)
					]),
				(config.tickLength > 0) ? _List_fromArray(
					[
						A2($terezka$elm_charts$Internal$Commands$Line, x2_ + tickOffsetX, y2_ + tickOffsetY)
					]) : _List_Nil));
		return A4(
			$terezka$elm_charts$Internal$Svg$withAttrs,
			config.attrs,
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('elm-charts__line'),
					$elm$svg$Svg$Attributes$fill('transparent'),
					$elm$svg$Svg$Attributes$stroke(config.color),
					$elm$svg$Svg$Attributes$strokeWidth(
					$elm$core$String$fromFloat(config.width)),
					$elm$svg$Svg$Attributes$strokeOpacity(
					$elm$core$String$fromFloat(config.opacity)),
					$elm$svg$Svg$Attributes$strokeDasharray(
					A2(
						$elm$core$String$join,
						' ',
						A2($elm$core$List$map, $elm$core$String$fromFloat, config.dashed))),
					$elm$svg$Svg$Attributes$d(
					A2($terezka$elm_charts$Internal$Commands$description, plane, cmds))
				]),
			_List_Nil);
	});
var $terezka$elm_charts$Chart$Svg$line = F2(
	function (plane, edits) {
		return A2(
			$terezka$elm_charts$Internal$Svg$line,
			plane,
			A2($terezka$elm_charts$Internal$Helpers$apply, edits, $terezka$elm_charts$Internal$Svg$defaultLine));
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $terezka$elm_charts$Chart$Attributes$size = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{size: v});
	});
var $terezka$elm_charts$Chart$Attributes$width = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{width: v});
	});
var $terezka$elm_charts$Chart$Attributes$x1 = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{
				x1: $elm$core$Maybe$Just(v)
			});
	});
var $terezka$elm_charts$Chart$Attributes$y1 = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{
				y1: $elm$core$Maybe$Just(v)
			});
	});
var $terezka$elm_charts$Chart$grid = function (edits) {
	var config = A2(
		$terezka$elm_charts$Internal$Helpers$apply,
		edits,
		{color: '', dashed: _List_Nil, dotGrid: false, width: 0});
	var width = (!config.width) ? (config.dotGrid ? 0.5 : 1) : config.width;
	var color = $elm$core$String$isEmpty(config.color) ? (config.dotGrid ? $terezka$elm_charts$Internal$Helpers$darkGray : $terezka$elm_charts$Internal$Helpers$gray) : config.color;
	var toDot = F4(
		function (vs, p, x, y) {
			return (A2($elm$core$List$member, x, vs.xAxis) || A2($elm$core$List$member, y, vs.yAxis)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				A5(
					$terezka$elm_charts$Chart$Svg$dot,
					p,
					function ($) {
						return $.x;
					},
					function ($) {
						return $.y;
					},
					_List_fromArray(
						[
							$terezka$elm_charts$Chart$Attributes$color(color),
							$terezka$elm_charts$Chart$Attributes$size(width),
							$terezka$elm_charts$Chart$Attributes$circle
						]),
					{x: x, y: y}));
		});
	var toXGrid = F3(
		function (vs, p, v) {
			return A2($elm$core$List$member, v, vs.xAxis) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				A2(
					$terezka$elm_charts$Chart$Svg$line,
					p,
					_List_fromArray(
						[
							$terezka$elm_charts$Chart$Attributes$color(color),
							$terezka$elm_charts$Chart$Attributes$width(width),
							$terezka$elm_charts$Chart$Attributes$x1(v),
							$terezka$elm_charts$Chart$Attributes$dashed(config.dashed)
						])));
		});
	var toYGrid = F3(
		function (vs, p, v) {
			return A2($elm$core$List$member, v, vs.yAxis) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				A2(
					$terezka$elm_charts$Chart$Svg$line,
					p,
					_List_fromArray(
						[
							$terezka$elm_charts$Chart$Attributes$color(color),
							$terezka$elm_charts$Chart$Attributes$width(width),
							$terezka$elm_charts$Chart$Attributes$y1(v),
							$terezka$elm_charts$Chart$Attributes$dashed(config.dashed)
						])));
		});
	return $terezka$elm_charts$Chart$GridElement(
		F2(
			function (p, vs) {
				return A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('elm-charts__grid')
						]),
					config.dotGrid ? A2(
						$elm$core$List$concatMap,
						function (x) {
							return A2(
								$elm$core$List$filterMap,
								A3(toDot, vs, p, x),
								vs.ys);
						},
						vs.xs) : _List_fromArray(
						[
							A2(
							$elm$svg$Svg$g,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$class('elm-charts__x-grid')
								]),
							A2(
								$elm$core$List$filterMap,
								A2(toXGrid, vs, p),
								vs.xs)),
							A2(
							$elm$svg$Svg$g,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$class('elm-charts__y-grid')
								]),
							A2(
								$elm$core$List$filterMap,
								A2(toYGrid, vs, p),
								vs.ys))
						]));
			}));
};
var $elm$svg$Svg$Attributes$style = _VirtualDom_attribute('style');
var $terezka$elm_charts$Chart$viewElements = F6(
	function (config, plane, tickValues, allItems, allLegends, elements) {
		var viewOne = F2(
			function (el, _v0) {
				var before = _v0.a;
				var chart_ = _v0.b;
				var after = _v0.c;
				switch (el.$) {
					case 'Indexed':
						return _Utils_Tuple3(before, chart_, after);
					case 'SeriesElement':
						var view = el.d;
						return _Utils_Tuple3(
							before,
							A2(
								$elm$core$List$cons,
								view(plane),
								chart_),
							after);
					case 'BarsElement':
						var view = el.e;
						return _Utils_Tuple3(
							before,
							A2(
								$elm$core$List$cons,
								view(plane),
								chart_),
							after);
					case 'CustomElement':
						var view = el.b;
						return _Utils_Tuple3(
							before,
							A2(
								$elm$core$List$cons,
								view(plane),
								chart_),
							after);
					case 'AxisElement':
						var view = el.b;
						return _Utils_Tuple3(
							before,
							A2(
								$elm$core$List$cons,
								view(plane),
								chart_),
							after);
					case 'TicksElement':
						var view = el.b;
						return _Utils_Tuple3(
							before,
							A2(
								$elm$core$List$cons,
								view(plane),
								chart_),
							after);
					case 'TickElement':
						var toC = el.a;
						var view = el.c;
						return _Utils_Tuple3(
							before,
							A2(
								$elm$core$List$cons,
								A2(
									view,
									plane,
									toC(plane)),
								chart_),
							after);
					case 'LabelsElement':
						var toC = el.a;
						var view = el.c;
						return _Utils_Tuple3(
							before,
							A2(
								$elm$core$List$cons,
								A2(
									view,
									plane,
									toC(plane)),
								chart_),
							after);
					case 'LabelElement':
						var toC = el.a;
						var view = el.c;
						return _Utils_Tuple3(
							before,
							A2(
								$elm$core$List$cons,
								A2(
									view,
									plane,
									toC(plane)),
								chart_),
							after);
					case 'GridElement':
						var view = el.a;
						return _Utils_Tuple3(
							before,
							A2(
								$elm$core$List$cons,
								A2(view, plane, tickValues),
								chart_),
							after);
					case 'SubElements':
						var func = el.a;
						return A3(
							$elm$core$List$foldr,
							viewOne,
							_Utils_Tuple3(before, chart_, after),
							A2(func, plane, allItems));
					case 'ListOfElements':
						var els = el.a;
						return A3(
							$elm$core$List$foldr,
							viewOne,
							_Utils_Tuple3(before, chart_, after),
							els);
					case 'SvgElement':
						var view = el.a;
						return _Utils_Tuple3(
							before,
							A2(
								$elm$core$List$cons,
								view(plane),
								chart_),
							after);
					default:
						var view = el.a;
						return _Utils_Tuple3(
							($elm$core$List$length(chart_) > 0) ? A2(
								$elm$core$List$cons,
								A2(view, plane, allLegends),
								before) : before,
							chart_,
							($elm$core$List$length(chart_) > 0) ? after : A2(
								$elm$core$List$cons,
								A2(view, plane, allLegends),
								after));
				}
			});
		return A3(
			$elm$core$List$foldr,
			viewOne,
			_Utils_Tuple3(_List_Nil, _List_Nil, _List_Nil),
			elements);
	});
var $terezka$elm_charts$Chart$chart = F2(
	function (edits, unindexedElements) {
		var indexedElements = function () {
			var toIndexedEl = F2(
				function (el, _v4) {
					var acc = _v4.a;
					var index = _v4.b;
					switch (el.$) {
						case 'Indexed':
							var toElAndIndex = el.a;
							var _v6 = toElAndIndex(index);
							var newEl = _v6.a;
							var newIndex = _v6.b;
							return _Utils_Tuple2(
								_Utils_ap(
									acc,
									_List_fromArray(
										[newEl])),
								newIndex);
						case 'ListOfElements':
							var els = el.a;
							return A3(
								$elm$core$List$foldl,
								toIndexedEl,
								_Utils_Tuple2(acc, index),
								els);
						default:
							return _Utils_Tuple2(
								_Utils_ap(
									acc,
									_List_fromArray(
										[el])),
								index);
					}
				});
			return A3(
				$elm$core$List$foldl,
				toIndexedEl,
				_Utils_Tuple2(_List_Nil, 0),
				unindexedElements).a;
		}();
		var elements = function () {
			var isGrid = function (el) {
				if (el.$ === 'GridElement') {
					return true;
				} else {
					return false;
				}
			};
			return A2($elm$core$List$any, isGrid, indexedElements) ? indexedElements : A2(
				$elm$core$List$cons,
				$terezka$elm_charts$Chart$grid(_List_Nil),
				indexedElements);
		}();
		var legends_ = $terezka$elm_charts$Chart$getLegends(elements);
		var config = A2(
			$terezka$elm_charts$Internal$Helpers$apply,
			edits,
			{
				attrs: _List_fromArray(
					[
						$elm$svg$Svg$Attributes$style('overflow: visible;')
					]),
				domain: _List_Nil,
				events: _List_Nil,
				height: 300,
				htmlAttrs: _List_Nil,
				margin: {bottom: 0, left: 0, right: 0, top: 0},
				padding: {bottom: 0, left: 0, right: 0, top: 0},
				range: _List_Nil,
				responsive: true,
				width: 300
			});
		var plane = A2($terezka$elm_charts$Chart$definePlane, config, elements);
		var items = A2($terezka$elm_charts$Chart$getItems, plane, elements);
		var toEvent = function (_v2) {
			var event_ = _v2.a;
			var _v1 = event_.decoder;
			var decoder = _v1.a;
			return A2(
				$terezka$elm_charts$Internal$Svg$Event,
				event_.name,
				decoder(items));
		};
		var tickValues = A3($terezka$elm_charts$Chart$getTickValues, plane, items, elements);
		var _v0 = A6($terezka$elm_charts$Chart$viewElements, config, plane, tickValues, items, legends_, elements);
		var beforeEls = _v0.a;
		var chartEls = _v0.b;
		var afterEls = _v0.c;
		return A5(
			$terezka$elm_charts$Internal$Svg$container,
			plane,
			{
				attrs: config.attrs,
				events: A2($elm$core$List$map, toEvent, config.events),
				htmlAttrs: config.htmlAttrs,
				responsive: config.responsive
			},
			beforeEls,
			chartEls,
			afterEls);
	});
var $author$project$DataConvert$funcMaybeToMaybeFunc = F2(
	function (datum, func) {
		var _v0 = func(datum);
		if (_v0.$ === 'Just') {
			var val = _v0.a;
			return $elm$core$Maybe$Just(
				function (s) {
					return A2(
						$elm$core$Maybe$withDefault,
						val,
						func(s));
				});
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $terezka$elm_charts$Internal$Svg$Linear = {$: 'Linear'};
var $terezka$elm_charts$Chart$Attributes$linear = function (config) {
	return _Utils_update(
		config,
		{
			method: $elm$core$Maybe$Just($terezka$elm_charts$Internal$Svg$Linear)
		});
};
var $terezka$elm_charts$Internal$Property$Property = function (a) {
	return {$: 'Property', a: a};
};
var $terezka$elm_charts$Internal$Property$property = F3(
	function (value, inter, attrs) {
		return $terezka$elm_charts$Internal$Property$Property(
			{
				attrs: attrs,
				extra: F5(
					function (_v0, _v1, _v2, _v3, _v4) {
						return _List_Nil;
					}),
				format: A2(
					$elm$core$Basics$composeR,
					value,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Maybe$map($elm$core$String$fromFloat),
						$elm$core$Maybe$withDefault('N/A'))),
				inter: inter,
				meta: $elm$core$Maybe$Nothing,
				value: value,
				visual: value
			});
	});
var $terezka$elm_charts$Chart$interpolated = F2(
	function (y, inter) {
		return A2(
			$terezka$elm_charts$Internal$Property$property,
			A2($elm$core$Basics$composeR, y, $elm$core$Maybe$Just),
			_Utils_ap(
				_List_fromArray(
					[$terezka$elm_charts$Chart$Attributes$linear]),
				inter));
	});
var $terezka$elm_charts$Internal$Property$Stacked = function (a) {
	return {$: 'Stacked', a: a};
};
var $terezka$elm_charts$Internal$Property$meta = F2(
	function (value, prop) {
		if (prop.$ === 'Property') {
			var con = prop.a;
			return $terezka$elm_charts$Internal$Property$Property(
				_Utils_update(
					con,
					{
						meta: $elm$core$Maybe$Just(value)
					}));
		} else {
			var cons = prop.a;
			return $terezka$elm_charts$Internal$Property$Stacked(
				A2(
					$elm$core$List$map,
					function (con) {
						return _Utils_update(
							con,
							{
								meta: $elm$core$Maybe$Just(value)
							});
					},
					cons));
		}
	});
var $terezka$elm_charts$Chart$named = function (name) {
	return $terezka$elm_charts$Internal$Property$meta(name);
};
var $author$project$MyCharts$Chart$Curve$optionToNameCurve = F2(
	function (chartDatum, optStr) {
		switch (chartDatum.$) {
			case 'TS1':
				var datum = chartDatum.a;
				switch (optStr) {
					case 't':
						return 't';
					case 'x1':
						return 'x';
					default:
						return 'erro';
				}
			case 'TS1E1R1U1':
				var datum = chartDatum.a;
				switch (optStr) {
					case 't':
						return 't';
					case 'x1':
						return 'x';
					case 'e1':
						return 'e';
					case 'r1':
						return 'r';
					case 'u1':
						return 'u';
					default:
						return 'erro';
				}
			case 'TS1E1R1U4':
				var datum = chartDatum.a;
				switch (optStr) {
					case 't':
						return 't';
					case 'x1':
						return 'x';
					case 'e1':
						return 'e';
					case 'r1':
						return 'r';
					case 'u1':
						return 'u';
					case 'u2':
						return 'up';
					case 'u3':
						return 'ui';
					case 'u4':
						return 'ud';
					default:
						return 'erro';
				}
			default:
				var datum = chartDatum.a;
				switch (optStr) {
					case 't':
						return 't';
					case 'x1':
						return 'x1';
					case 'x2':
						return 'x2';
					case 'e1':
						return 'e';
					case 'r1':
						return 'r';
					case 'u1':
						return 'u';
					case 'u2':
						return 'up';
					case 'u3':
						return 'ui';
					case 'u4':
						return 'ud';
					default:
						return 'erro';
				}
		}
	});
var $terezka$elm_charts$Chart$Indexed = function (a) {
	return {$: 'Indexed', a: a};
};
var $terezka$elm_charts$Chart$SeriesElement = F4(
	function (a, b, c, d) {
		return {$: 'SeriesElement', a: a, b: b, c: c, d: d};
	});
var $terezka$elm_charts$Internal$Item$Rendered = function (a) {
	return {$: 'Rendered', a: a};
};
var $terezka$elm_charts$Internal$Item$toHtml = function (_v0) {
	var item = _v0.a;
	return item.toHtml(item.config);
};
var $terezka$elm_charts$Internal$Item$toSvg = F2(
	function (plane, _v0) {
		var item = _v0.a;
		return A3(
			item.toSvg,
			plane,
			item.config,
			A2(item.toPosition, plane, item.config));
	});
var $terezka$elm_charts$Internal$Item$generalize = F2(
	function (toAny, _v0) {
		var item = _v0.a;
		return $terezka$elm_charts$Internal$Item$Rendered(
			{
				config: {
					product: toAny(item.config.product),
					toAny: $elm$core$Basics$identity,
					tooltipInfo: item.config.tooltipInfo,
					values: item.config.values
				},
				toHtml: function (c) {
					return $terezka$elm_charts$Internal$Item$toHtml(
						$terezka$elm_charts$Internal$Item$Rendered(item));
				},
				toLimits: function (_v1) {
					return item.toLimits(item.config);
				},
				toPosition: F2(
					function (plane, _v2) {
						return A2(item.toPosition, plane, item.config);
					}),
				toSvg: F3(
					function (plane, _v3, _v4) {
						return A2(
							$terezka$elm_charts$Internal$Item$toSvg,
							plane,
							$terezka$elm_charts$Internal$Item$Rendered(item));
					})
			});
	});
var $terezka$elm_charts$Internal$Many$getMembers = function (_v0) {
	var group_ = _v0.a;
	return function (_v1) {
		var x = _v1.a;
		var xs = _v1.b;
		return A2($elm$core$List$cons, x, xs);
	}(group_.config.items);
};
var $terezka$elm_charts$Internal$Many$getGenerals = function (group_) {
	var generalize = function (_v0) {
		var item = _v0.a;
		return A2(
			$terezka$elm_charts$Internal$Item$generalize,
			item.config.toAny,
			$terezka$elm_charts$Internal$Item$Rendered(item));
	};
	return A2(
		$elm$core$List$map,
		generalize,
		$terezka$elm_charts$Internal$Many$getMembers(group_));
};
var $terezka$elm_charts$Internal$Item$getLimits = function (_v0) {
	var item = _v0.a;
	return item.toLimits(item.config);
};
var $terezka$elm_charts$Internal$Item$map = F2(
	function (func, _v0) {
		var item = _v0.a;
		return $terezka$elm_charts$Internal$Item$Rendered(
			{
				config: {
					product: item.config.product,
					toAny: item.config.toAny,
					tooltipInfo: item.config.tooltipInfo,
					values: {
						datum: func(item.config.values.datum),
						isReal: item.config.values.isReal,
						x1: item.config.values.x1,
						x2: item.config.values.x2,
						y: item.config.values.y
					}
				},
				toHtml: function (_v1) {
					return $terezka$elm_charts$Internal$Item$toHtml(
						$terezka$elm_charts$Internal$Item$Rendered(item));
				},
				toLimits: function (_v2) {
					return item.toLimits(item.config);
				},
				toPosition: F2(
					function (plane, _v3) {
						return A2(item.toPosition, plane, item.config);
					}),
				toSvg: F3(
					function (plane, _v4, _v5) {
						return A2(
							$terezka$elm_charts$Internal$Item$toSvg,
							plane,
							$terezka$elm_charts$Internal$Item$Rendered(item));
					})
			});
	});
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$svg$Svg$map = $elm$virtual_dom$VirtualDom$map;
var $terezka$elm_charts$Internal$Property$toConfigs = function (prop) {
	if (prop.$ === 'Property') {
		var config = prop.a;
		return _List_fromArray(
			[config]);
	} else {
		var configs = prop.a;
		return configs;
	}
};
var $terezka$elm_charts$Internal$Legend$LineLegend = F3(
	function (a, b, c) {
		return {$: 'LineLegend', a: a, b: b, c: c};
	});
var $terezka$elm_charts$Chart$Attributes$border = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{border: v});
	});
var $terezka$elm_charts$Internal$Svg$defaultInterpolation = {attrs: _List_Nil, color: $terezka$elm_charts$Internal$Helpers$pink, dashed: _List_Nil, design: $elm$core$Maybe$Nothing, method: $elm$core$Maybe$Nothing, opacity: 0, width: 1};
var $terezka$elm_charts$Chart$Attributes$opacity = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{opacity: v});
	});
var $terezka$elm_charts$Internal$Helpers$blue = '#12A5ED';
var $terezka$elm_charts$Internal$Helpers$green = '#71c614';
var $terezka$elm_charts$Internal$Helpers$orange = '#FF8400';
var $terezka$elm_charts$Internal$Helpers$purple = '#7b4dff';
var $terezka$elm_charts$Internal$Helpers$red = '#F5325B';
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $terezka$elm_charts$Internal$Helpers$toDefault = F3(
	function (_default, items, index) {
		var dict = $elm$core$Dict$fromList(
			A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, items));
		var numOfItems = $elm$core$Dict$size(dict);
		var itemIndex = index % numOfItems;
		return A2(
			$elm$core$Maybe$withDefault,
			_default,
			A2($elm$core$Dict$get, itemIndex, dict));
	});
var $terezka$elm_charts$Internal$Helpers$yellow = '#FFCA00';
var $terezka$elm_charts$Internal$Helpers$toDefaultColor = A2(
	$terezka$elm_charts$Internal$Helpers$toDefault,
	$terezka$elm_charts$Internal$Helpers$pink,
	_List_fromArray(
		[$terezka$elm_charts$Internal$Helpers$purple, $terezka$elm_charts$Internal$Helpers$pink, $terezka$elm_charts$Internal$Helpers$blue, $terezka$elm_charts$Internal$Helpers$green, $terezka$elm_charts$Internal$Helpers$red, $terezka$elm_charts$Internal$Helpers$yellow, $terezka$elm_charts$Internal$Helpers$orange]));
var $terezka$elm_charts$Internal$Legend$toDotLegends = F2(
	function (elIndex, properties) {
		var toInterConfig = function (attrs) {
			return A2($terezka$elm_charts$Internal$Helpers$apply, attrs, $terezka$elm_charts$Internal$Svg$defaultInterpolation);
		};
		var toDotLegend = F3(
			function (props, prop, colorIndex) {
				var defaultOpacity = ($elm$core$List$length(props) > 1) ? 0.4 : 0;
				var interAttr = _Utils_ap(
					_List_fromArray(
						[
							$terezka$elm_charts$Chart$Attributes$color(
							$terezka$elm_charts$Internal$Helpers$toDefaultColor(colorIndex)),
							$terezka$elm_charts$Chart$Attributes$opacity(defaultOpacity)
						]),
					prop.inter);
				var interConfig = toInterConfig(interAttr);
				var defaultName = 'Property #' + $elm$core$String$fromInt(colorIndex + 1);
				var defaultAttrs = _List_fromArray(
					[
						$terezka$elm_charts$Chart$Attributes$color(interConfig.color),
						$terezka$elm_charts$Chart$Attributes$border(interConfig.color),
						_Utils_eq(interConfig.method, $elm$core$Maybe$Nothing) ? $terezka$elm_charts$Chart$Attributes$circle : $elm$core$Basics$identity
					]);
				var dotAttrs = _Utils_ap(defaultAttrs, prop.attrs);
				return A3(
					$terezka$elm_charts$Internal$Legend$LineLegend,
					A2($elm$core$Maybe$withDefault, defaultName, prop.meta),
					interAttr,
					dotAttrs);
			});
		return A2(
			$elm$core$List$indexedMap,
			F2(
				function (propIndex, f) {
					return f(elIndex + propIndex);
				}),
			A2(
				$elm$core$List$concatMap,
				function (ps) {
					return A2(
						$elm$core$List$map,
						toDotLegend(ps),
						ps);
				},
				A2($elm$core$List$map, $terezka$elm_charts$Internal$Property$toConfigs, properties)));
	});
var $terezka$elm_charts$Internal$Item$Dot = function (a) {
	return {$: 'Dot', a: a};
};
var $terezka$elm_charts$Internal$Svg$clipperStyle = F2(
	function (plane, limits) {
		var topCut = ((plane.y.max - limits.y2) / $elm$core$Basics$abs(limits.y2 - limits.y1)) * 100;
		var rightCut = ((plane.x.max - limits.x2) / $elm$core$Basics$abs(limits.x2 - limits.x1)) * 100;
		var leftCut = ((plane.x.min - limits.x1) / $elm$core$Basics$abs(limits.x2 - limits.x1)) * 100;
		var bottomCut = ((plane.y.min - limits.y1) / $elm$core$Basics$abs(limits.y2 - limits.y1)) * 100;
		var path = A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					$elm$core$String$fromFloat(-topCut) + '%',
					$elm$core$String$fromFloat(-rightCut) + '%',
					$elm$core$String$fromFloat(bottomCut) + '%',
					$elm$core$String$fromFloat(leftCut) + '%'
				]));
		return 'clip-path: inset(' + (path + ');');
	});
var $elm$svg$Svg$Attributes$fillRule = _VirtualDom_attribute('fill-rule');
var $terezka$elm_charts$Internal$Coordinates$fromProps = F3(
	function (xs, ys, data) {
		var getValues = F2(
			function (vs, datum) {
				return A2(
					$elm$core$List$filterMap,
					function (v) {
						return v(datum);
					},
					vs);
			});
		var getMin = A2(
			$elm$core$Basics$composeL,
			$elm$core$Maybe$withDefault(0),
			$elm$core$List$minimum);
		var getMax = A2(
			$elm$core$Basics$composeL,
			$elm$core$Maybe$withDefault(1),
			$elm$core$List$maximum);
		var toPosition = function (datum) {
			var vsY = A2(getValues, ys, datum);
			var vsX = A2(getValues, xs, datum);
			return {
				x1: getMin(vsX),
				x2: getMax(vsX),
				y1: getMin(vsY),
				y2: getMax(vsY)
			};
		};
		return A2($terezka$elm_charts$Internal$Coordinates$foldPosition, toPosition, data);
	});
var $terezka$elm_charts$Internal$Interpolation$linear = $elm$core$List$map(
	$elm$core$List$map(
		function (_v0) {
			var x = _v0.x;
			var y = _v0.y;
			return A2($terezka$elm_charts$Internal$Commands$Line, x, y);
		}));
var $terezka$elm_charts$Internal$Interpolation$First = {$: 'First'};
var $terezka$elm_charts$Internal$Interpolation$Previous = function (a) {
	return {$: 'Previous', a: a};
};
var $terezka$elm_charts$Internal$Interpolation$monotoneCurve = F4(
	function (point0, point1, tangent0, tangent1) {
		var dx = (point1.x - point0.x) / 3;
		return A6($terezka$elm_charts$Internal$Commands$CubicBeziers, point0.x + dx, point0.y + (dx * tangent0), point1.x - dx, point1.y - (dx * tangent1), point1.x, point1.y);
	});
var $terezka$elm_charts$Internal$Interpolation$slope2 = F3(
	function (point0, point1, t) {
		var h = point1.x - point0.x;
		return (!(!h)) ? ((((3 * (point1.y - point0.y)) / h) - t) / 2) : t;
	});
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $terezka$elm_charts$Internal$Interpolation$sign = function (x) {
	return (x < 0) ? (-1) : 1;
};
var $terezka$elm_charts$Internal$Interpolation$toH = F2(
	function (h0, h1) {
		return (!h0) ? ((h1 < 0) ? (0 * (-1)) : h1) : h0;
	});
var $terezka$elm_charts$Internal$Interpolation$slope3 = F3(
	function (point0, point1, point2) {
		var h1 = point2.x - point1.x;
		var h0 = point1.x - point0.x;
		var s0h = A2($terezka$elm_charts$Internal$Interpolation$toH, h0, h1);
		var s0 = (point1.y - point0.y) / s0h;
		var s1h = A2($terezka$elm_charts$Internal$Interpolation$toH, h1, h0);
		var s1 = (point2.y - point1.y) / s1h;
		var p = ((s0 * h1) + (s1 * h0)) / (h0 + h1);
		var slope = ($terezka$elm_charts$Internal$Interpolation$sign(s0) + $terezka$elm_charts$Internal$Interpolation$sign(s1)) * A2(
			$elm$core$Basics$min,
			A2(
				$elm$core$Basics$min,
				$elm$core$Basics$abs(s0),
				$elm$core$Basics$abs(s1)),
			0.5 * $elm$core$Basics$abs(p));
		return $elm$core$Basics$isNaN(slope) ? 0 : slope;
	});
var $terezka$elm_charts$Internal$Interpolation$monotonePart = F2(
	function (points, _v0) {
		var tangent = _v0.a;
		var commands = _v0.b;
		var _v1 = _Utils_Tuple2(tangent, points);
		_v1$4:
		while (true) {
			if (_v1.a.$ === 'First') {
				if (_v1.b.b && _v1.b.b.b) {
					if (_v1.b.b.b.b) {
						var _v2 = _v1.a;
						var _v3 = _v1.b;
						var p0 = _v3.a;
						var _v4 = _v3.b;
						var p1 = _v4.a;
						var _v5 = _v4.b;
						var p2 = _v5.a;
						var rest = _v5.b;
						var t1 = A3($terezka$elm_charts$Internal$Interpolation$slope3, p0, p1, p2);
						var t0 = A3($terezka$elm_charts$Internal$Interpolation$slope2, p0, p1, t1);
						return A2(
							$terezka$elm_charts$Internal$Interpolation$monotonePart,
							A2(
								$elm$core$List$cons,
								p1,
								A2($elm$core$List$cons, p2, rest)),
							_Utils_Tuple2(
								$terezka$elm_charts$Internal$Interpolation$Previous(t1),
								_Utils_ap(
									commands,
									_List_fromArray(
										[
											A4($terezka$elm_charts$Internal$Interpolation$monotoneCurve, p0, p1, t0, t1)
										]))));
					} else {
						var _v9 = _v1.a;
						var _v10 = _v1.b;
						var p0 = _v10.a;
						var _v11 = _v10.b;
						var p1 = _v11.a;
						var t1 = A3($terezka$elm_charts$Internal$Interpolation$slope3, p0, p1, p1);
						return _Utils_Tuple2(
							$terezka$elm_charts$Internal$Interpolation$Previous(t1),
							_Utils_ap(
								commands,
								_List_fromArray(
									[
										A4($terezka$elm_charts$Internal$Interpolation$monotoneCurve, p0, p1, t1, t1),
										A2($terezka$elm_charts$Internal$Commands$Line, p1.x, p1.y)
									])));
					}
				} else {
					break _v1$4;
				}
			} else {
				if (_v1.b.b && _v1.b.b.b) {
					if (_v1.b.b.b.b) {
						var t0 = _v1.a.a;
						var _v6 = _v1.b;
						var p0 = _v6.a;
						var _v7 = _v6.b;
						var p1 = _v7.a;
						var _v8 = _v7.b;
						var p2 = _v8.a;
						var rest = _v8.b;
						var t1 = A3($terezka$elm_charts$Internal$Interpolation$slope3, p0, p1, p2);
						return A2(
							$terezka$elm_charts$Internal$Interpolation$monotonePart,
							A2(
								$elm$core$List$cons,
								p1,
								A2($elm$core$List$cons, p2, rest)),
							_Utils_Tuple2(
								$terezka$elm_charts$Internal$Interpolation$Previous(t1),
								_Utils_ap(
									commands,
									_List_fromArray(
										[
											A4($terezka$elm_charts$Internal$Interpolation$monotoneCurve, p0, p1, t0, t1)
										]))));
					} else {
						var t0 = _v1.a.a;
						var _v12 = _v1.b;
						var p0 = _v12.a;
						var _v13 = _v12.b;
						var p1 = _v13.a;
						var t1 = A3($terezka$elm_charts$Internal$Interpolation$slope3, p0, p1, p1);
						return _Utils_Tuple2(
							$terezka$elm_charts$Internal$Interpolation$Previous(t1),
							_Utils_ap(
								commands,
								_List_fromArray(
									[
										A4($terezka$elm_charts$Internal$Interpolation$monotoneCurve, p0, p1, t0, t1),
										A2($terezka$elm_charts$Internal$Commands$Line, p1.x, p1.y)
									])));
					}
				} else {
					break _v1$4;
				}
			}
		}
		return _Utils_Tuple2(tangent, commands);
	});
var $terezka$elm_charts$Internal$Interpolation$monotoneSection = F2(
	function (points, _v0) {
		var tangent = _v0.a;
		var acc = _v0.b;
		var _v1 = function () {
			if (points.b) {
				var p0 = points.a;
				var rest = points.b;
				return A2(
					$terezka$elm_charts$Internal$Interpolation$monotonePart,
					A2($elm$core$List$cons, p0, rest),
					_Utils_Tuple2(
						tangent,
						_List_fromArray(
							[
								A2($terezka$elm_charts$Internal$Commands$Line, p0.x, p0.y)
							])));
			} else {
				return _Utils_Tuple2(tangent, _List_Nil);
			}
		}();
		var t0 = _v1.a;
		var commands = _v1.b;
		return _Utils_Tuple2(
			t0,
			A2($elm$core$List$cons, commands, acc));
	});
var $terezka$elm_charts$Internal$Interpolation$monotone = function (sections) {
	return A3(
		$elm$core$List$foldr,
		$terezka$elm_charts$Internal$Interpolation$monotoneSection,
		_Utils_Tuple2($terezka$elm_charts$Internal$Interpolation$First, _List_Nil),
		sections).b;
};
var $terezka$elm_charts$Internal$Interpolation$Point = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var $terezka$elm_charts$Internal$Interpolation$after = F2(
	function (a, b) {
		return _List_fromArray(
			[
				a,
				A2($terezka$elm_charts$Internal$Interpolation$Point, b.x, a.y),
				b
			]);
	});
var $terezka$elm_charts$Internal$Interpolation$stepped = function (sections) {
	var expand = F2(
		function (result, section) {
			expand:
			while (true) {
				if (section.b) {
					if (section.b.b) {
						var a = section.a;
						var _v1 = section.b;
						var b = _v1.a;
						var rest = _v1.b;
						var $temp$result = _Utils_ap(
							result,
							A2($terezka$elm_charts$Internal$Interpolation$after, a, b)),
							$temp$section = A2($elm$core$List$cons, b, rest);
						result = $temp$result;
						section = $temp$section;
						continue expand;
					} else {
						var last = section.a;
						return result;
					}
				} else {
					return result;
				}
			}
		});
	return A2(
		$elm$core$List$map,
		A2(
			$elm$core$Basics$composeR,
			expand(_List_Nil),
			$elm$core$List$map(
				function (_v2) {
					var x = _v2.x;
					var y = _v2.y;
					return A2($terezka$elm_charts$Internal$Commands$Line, x, y);
				})),
		sections);
};
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $terezka$elm_charts$Internal$Svg$last = function (list) {
	return $elm$core$List$head(
		A2(
			$elm$core$List$drop,
			$elm$core$List$length(list) - 1,
			list));
};
var $terezka$elm_charts$Internal$Svg$withBorder = F2(
	function (stuff, func) {
		if (stuff.b) {
			var first = stuff.a;
			var rest = stuff.b;
			return $elm$core$Maybe$Just(
				A2(
					func,
					first,
					A2(
						$elm$core$Maybe$withDefault,
						first,
						$terezka$elm_charts$Internal$Svg$last(rest))));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $terezka$elm_charts$Internal$Svg$toCommands = F4(
	function (method, toX, toY, data) {
		var toSets = F2(
			function (ps, cmds) {
				return A2(
					$terezka$elm_charts$Internal$Svg$withBorder,
					ps,
					F2(
						function (first, last_) {
							return _Utils_Tuple3(first, cmds, last_);
						}));
			});
		var fold = F2(
			function (datum_, acc) {
				var _v1 = toY(datum_);
				if (_v1.$ === 'Just') {
					var y_ = _v1.a;
					if (acc.b) {
						var latest = acc.a;
						var rest = acc.b;
						return A2(
							$elm$core$List$cons,
							_Utils_ap(
								latest,
								_List_fromArray(
									[
										{
										x: toX(datum_),
										y: y_
									}
									])),
							rest);
					} else {
						return A2(
							$elm$core$List$cons,
							_List_fromArray(
								[
									{
									x: toX(datum_),
									y: y_
								}
								]),
							acc);
					}
				} else {
					return A2($elm$core$List$cons, _List_Nil, acc);
				}
			});
		var points = $elm$core$List$reverse(
			A3($elm$core$List$foldl, fold, _List_Nil, data));
		var commands = function () {
			switch (method.$) {
				case 'Linear':
					return $terezka$elm_charts$Internal$Interpolation$linear(points);
				case 'Monotone':
					return $terezka$elm_charts$Internal$Interpolation$monotone(points);
				default:
					return $terezka$elm_charts$Internal$Interpolation$stepped(points);
			}
		}();
		return A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			A3($elm$core$List$map2, toSets, points, commands));
	});
var $terezka$elm_charts$Internal$Svg$apply = F2(
	function (funcs, _default) {
		var apply_ = F2(
			function (f, a) {
				return f(a);
			});
		return A3($elm$core$List$foldl, apply_, _default, funcs);
	});
var $elm$svg$Svg$defs = $elm$svg$Svg$trustedNode('defs');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $elm$svg$Svg$linearGradient = $elm$svg$Svg$trustedNode('linearGradient');
var $elm$svg$Svg$Attributes$offset = _VirtualDom_attribute('offset');
var $elm$svg$Svg$pattern = $elm$svg$Svg$trustedNode('pattern');
var $elm$svg$Svg$Attributes$patternTransform = _VirtualDom_attribute('patternTransform');
var $elm$svg$Svg$Attributes$patternUnits = _VirtualDom_attribute('patternUnits');
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $elm$svg$Svg$stop = $elm$svg$Svg$trustedNode('stop');
var $elm$svg$Svg$Attributes$stopColor = _VirtualDom_attribute('stop-color');
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $terezka$elm_charts$Internal$Svg$toPattern = F2(
	function (defaultColor, design) {
		var toPatternId = function (props) {
			return A3(
				$elm$core$String$replace,
				'(',
				'-',
				A3(
					$elm$core$String$replace,
					')',
					'-',
					A3(
						$elm$core$String$replace,
						'.',
						'-',
						A3(
							$elm$core$String$replace,
							',',
							'-',
							A3(
								$elm$core$String$replace,
								' ',
								'-',
								A2(
									$elm$core$String$join,
									'-',
									_Utils_ap(
										_List_fromArray(
											[
												'elm-charts__pattern',
												function () {
												switch (design.$) {
													case 'Striped':
														return 'striped';
													case 'Dotted':
														return 'dotted';
													default:
														return 'gradient';
												}
											}()
											]),
										props)))))));
		};
		var toPatternDefs = F4(
			function (id, spacing, rotate, inside) {
				return A2(
					$elm$svg$Svg$defs,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$pattern,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$id(id),
									$elm$svg$Svg$Attributes$patternUnits('userSpaceOnUse'),
									$elm$svg$Svg$Attributes$width(
									$elm$core$String$fromFloat(spacing)),
									$elm$svg$Svg$Attributes$height(
									$elm$core$String$fromFloat(spacing)),
									$elm$svg$Svg$Attributes$patternTransform(
									'rotate(' + ($elm$core$String$fromFloat(rotate) + ')'))
								]),
							_List_fromArray(
								[inside]))
						]));
			});
		var _v0 = function () {
			switch (design.$) {
				case 'Striped':
					var edits = design.a;
					var config = A2(
						$terezka$elm_charts$Internal$Svg$apply,
						edits,
						{color: defaultColor, rotate: 45, spacing: 4, width: 3});
					var theId = toPatternId(
						_List_fromArray(
							[
								config.color,
								$elm$core$String$fromFloat(config.width),
								$elm$core$String$fromFloat(config.spacing),
								$elm$core$String$fromFloat(config.rotate)
							]));
					return _Utils_Tuple2(
						A4(
							toPatternDefs,
							theId,
							config.spacing,
							config.rotate,
							A2(
								$elm$svg$Svg$line,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$x1('0'),
										$elm$svg$Svg$Attributes$y('0'),
										$elm$svg$Svg$Attributes$x2('0'),
										$elm$svg$Svg$Attributes$y2(
										$elm$core$String$fromFloat(config.spacing)),
										$elm$svg$Svg$Attributes$stroke(config.color),
										$elm$svg$Svg$Attributes$strokeWidth(
										$elm$core$String$fromFloat(config.width))
									]),
								_List_Nil)),
						theId);
				case 'Dotted':
					var edits = design.a;
					var config = A2(
						$terezka$elm_charts$Internal$Svg$apply,
						edits,
						{color: defaultColor, rotate: 45, spacing: 4, width: 3});
					var theId = toPatternId(
						_List_fromArray(
							[
								config.color,
								$elm$core$String$fromFloat(config.width),
								$elm$core$String$fromFloat(config.spacing),
								$elm$core$String$fromFloat(config.rotate)
							]));
					return _Utils_Tuple2(
						A4(
							toPatternDefs,
							theId,
							config.spacing,
							config.rotate,
							A2(
								$elm$svg$Svg$circle,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$fill(config.color),
										$elm$svg$Svg$Attributes$cx(
										$elm$core$String$fromFloat(config.width / 3)),
										$elm$svg$Svg$Attributes$cy(
										$elm$core$String$fromFloat(config.width / 3)),
										$elm$svg$Svg$Attributes$r(
										$elm$core$String$fromFloat(config.width / 3))
									]),
								_List_Nil)),
						theId);
				default:
					var edits = design.a;
					var colors = _Utils_eq(edits, _List_Nil) ? _List_fromArray(
						[defaultColor, 'white']) : edits;
					var theId = toPatternId(colors);
					var totalColors = $elm$core$List$length(colors);
					var toPercentage = function (i) {
						return (i * 100) / (totalColors - 1);
					};
					var toStop = F2(
						function (i, c) {
							return A2(
								$elm$svg$Svg$stop,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$offset(
										$elm$core$String$fromFloat(
											toPercentage(i)) + '%'),
										$elm$svg$Svg$Attributes$stopColor(c)
									]),
								_List_Nil);
						});
					return _Utils_Tuple2(
						A2(
							$elm$svg$Svg$defs,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$svg$Svg$linearGradient,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$id(theId),
											$elm$svg$Svg$Attributes$x1('0'),
											$elm$svg$Svg$Attributes$x2('0'),
											$elm$svg$Svg$Attributes$y1('0'),
											$elm$svg$Svg$Attributes$y2('1')
										]),
									A2($elm$core$List$indexedMap, toStop, colors))
								])),
						theId);
			}
		}();
		var patternDefs = _v0.a;
		var patternId = _v0.b;
		return _Utils_Tuple2(patternDefs, 'url(#' + (patternId + ')'));
	});
var $terezka$elm_charts$Internal$Svg$area = F6(
	function (plane, toX, toY2M, toY, config, data) {
		var limits = A3(
			$terezka$elm_charts$Internal$Coordinates$fromProps,
			_List_fromArray(
				[
					A2($elm$core$Basics$composeR, toX, $elm$core$Maybe$Just)
				]),
			_List_fromArray(
				[
					toY,
					A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Basics$always(
						$elm$core$Maybe$Just(0)),
					toY2M)
				]),
			data);
		var _v0 = function () {
			var _v1 = config.design;
			if (_v1.$ === 'Nothing') {
				return _Utils_Tuple2(
					$elm$svg$Svg$text(''),
					config.color);
			} else {
				var design = _v1.a;
				return A2($terezka$elm_charts$Internal$Svg$toPattern, config.color, design);
			}
		}();
		var patternDefs = _v0.a;
		var fill = _v0.b;
		var view = function (cmds) {
			return A4(
				$terezka$elm_charts$Internal$Svg$withAttrs,
				config.attrs,
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('elm-charts__area-section'),
						$elm$svg$Svg$Attributes$fill(fill),
						$elm$svg$Svg$Attributes$fillOpacity(
						$elm$core$String$fromFloat(config.opacity)),
						$elm$svg$Svg$Attributes$strokeWidth('0'),
						$elm$svg$Svg$Attributes$fillRule('evenodd'),
						$elm$svg$Svg$Attributes$d(
						A2($terezka$elm_charts$Internal$Commands$description, plane, cmds)),
						$elm$svg$Svg$Attributes$style(
						A2($terezka$elm_charts$Internal$Svg$clipperStyle, plane, limits))
					]),
				_List_Nil);
		};
		var withUnder = F2(
			function (_v5, _v6) {
				var firstBottom = _v5.a;
				var cmdsBottom = _v5.b;
				var endBottom = _v5.c;
				var firstTop = _v6.a;
				var cmdsTop = _v6.b;
				var endTop = _v6.c;
				return view(
					_Utils_ap(
						_List_fromArray(
							[
								A2($terezka$elm_charts$Internal$Commands$Move, firstBottom.x, firstBottom.y),
								A2($terezka$elm_charts$Internal$Commands$Line, firstTop.x, firstTop.y)
							]),
						_Utils_ap(
							cmdsTop,
							_Utils_ap(
								_List_fromArray(
									[
										A2($terezka$elm_charts$Internal$Commands$Move, firstBottom.x, firstBottom.y)
									]),
								_Utils_ap(
									cmdsBottom,
									_List_fromArray(
										[
											A2($terezka$elm_charts$Internal$Commands$Line, endTop.x, endTop.y)
										]))))));
			});
		var withoutUnder = function (_v4) {
			var first = _v4.a;
			var cmds = _v4.b;
			var end = _v4.c;
			return view(
				_Utils_ap(
					_List_fromArray(
						[
							A2($terezka$elm_charts$Internal$Commands$Move, first.x, 0),
							A2($terezka$elm_charts$Internal$Commands$Line, first.x, first.y)
						]),
					_Utils_ap(
						cmds,
						_List_fromArray(
							[
								A2($terezka$elm_charts$Internal$Commands$Line, end.x, 0)
							]))));
		};
		if (config.opacity <= 0) {
			return $elm$svg$Svg$text('');
		} else {
			var _v2 = config.method;
			if (_v2.$ === 'Nothing') {
				return $elm$svg$Svg$text('');
			} else {
				var method = _v2.a;
				return A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('elm-charts__area-sections')
						]),
					function () {
						if (toY2M.$ === 'Nothing') {
							return A2(
								$elm$core$List$cons,
								patternDefs,
								A2(
									$elm$core$List$map,
									withoutUnder,
									A4($terezka$elm_charts$Internal$Svg$toCommands, method, toX, toY, data)));
						} else {
							var toY2 = toY2M.a;
							return A2(
								$elm$core$List$cons,
								patternDefs,
								A3(
									$elm$core$List$map2,
									withUnder,
									A4($terezka$elm_charts$Internal$Svg$toCommands, method, toX, toY2, data),
									A4($terezka$elm_charts$Internal$Svg$toCommands, method, toX, toY, data)));
						}
					}());
			}
		}
	});
var $terezka$elm_charts$Internal$Item$getPosition = F2(
	function (plane, _v0) {
		var item = _v0.a;
		return A2(item.toPosition, plane, item.config);
	});
var $terezka$elm_charts$Internal$Svg$interpolation = F5(
	function (plane, toX, toY, config, data) {
		var limits = A3(
			$terezka$elm_charts$Internal$Coordinates$fromProps,
			_List_fromArray(
				[
					A2($elm$core$Basics$composeR, toX, $elm$core$Maybe$Just)
				]),
			_List_fromArray(
				[toY]),
			data);
		var view = function (_v1) {
			var first = _v1.a;
			var cmds = _v1.b;
			return A4(
				$terezka$elm_charts$Internal$Svg$withAttrs,
				config.attrs,
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('elm-charts__interpolation-section'),
						$elm$svg$Svg$Attributes$fill('transparent'),
						$elm$svg$Svg$Attributes$stroke(config.color),
						$elm$svg$Svg$Attributes$strokeDasharray(
						A2(
							$elm$core$String$join,
							' ',
							A2($elm$core$List$map, $elm$core$String$fromFloat, config.dashed))),
						$elm$svg$Svg$Attributes$strokeWidth(
						$elm$core$String$fromFloat(config.width)),
						$elm$svg$Svg$Attributes$d(
						A2(
							$terezka$elm_charts$Internal$Commands$description,
							plane,
							A2(
								$elm$core$List$cons,
								A2($terezka$elm_charts$Internal$Commands$Move, first.x, first.y),
								cmds))),
						$elm$svg$Svg$Attributes$style(
						A2($terezka$elm_charts$Internal$Svg$clipperStyle, plane, limits))
					]),
				_List_Nil);
		};
		var _v0 = config.method;
		if (_v0.$ === 'Nothing') {
			return $elm$svg$Svg$text('');
		} else {
			var method = _v0.a;
			return A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('elm-charts__interpolation-sections')
					]),
				A2(
					$elm$core$List$map,
					view,
					A4($terezka$elm_charts$Internal$Svg$toCommands, method, toX, toY, data)));
		}
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $elm$html$Html$table = _VirtualDom_node('table');
var $terezka$elm_charts$Internal$Produce$toDefaultName = F2(
	function (index, name) {
		return A2(
			$elm$core$Maybe$withDefault,
			'Property #' + $elm$core$String$fromInt(index + 1),
			name);
	});
var $terezka$elm_charts$Internal$Svg$toRadius = F2(
	function (size_, shape) {
		var area_ = (2 * $elm$core$Basics$pi) * size_;
		switch (shape.$) {
			case 'Circle':
				return $elm$core$Basics$sqrt(area_ / $elm$core$Basics$pi);
			case 'Triangle':
				var side = $elm$core$Basics$sqrt(
					(area_ * 4) / $elm$core$Basics$sqrt(3));
				return $elm$core$Basics$sqrt(3) * side;
			case 'Square':
				return $elm$core$Basics$sqrt(area_) / 2;
			case 'Diamond':
				return $elm$core$Basics$sqrt(area_) / 2;
			case 'Cross':
				return $elm$core$Basics$sqrt(area_ / 4);
			default:
				return $elm$core$Basics$sqrt(area_ / 4);
		}
	});
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $terezka$elm_charts$Internal$Produce$tooltipRow = F3(
	function (color, title, text) {
		return A2(
			$elm$html$Html$tr,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$td,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'color', color),
							A2($elm$html$Html$Attributes$style, 'padding', '0'),
							A2($elm$html$Html$Attributes$style, 'padding-right', '3px')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(title + ':')
						])),
					A2(
					$elm$html$Html$td,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'text-align', 'right'),
							A2($elm$html$Html$Attributes$style, 'padding', '0')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(text)
						]))
				]));
	});
var $terezka$elm_charts$Internal$Produce$toDotSeries = F4(
	function (elIndex, toX, properties, data) {
		var toInterConfig = function (attrs) {
			return A2($terezka$elm_charts$Internal$Helpers$apply, attrs, $terezka$elm_charts$Internal$Svg$defaultInterpolation);
		};
		var toDotConfig = function (attrs) {
			return A2($terezka$elm_charts$Internal$Helpers$apply, attrs, $terezka$elm_charts$Internal$Svg$defaultDot);
		};
		var toDotItem = F7(
			function (lineIndex, stackIndex, colorIndex, prop, interConfig, dataIndex, datum_) {
				var y_ = A2(
					$elm$core$Maybe$withDefault,
					0,
					prop.visual(datum_));
				var x_ = toX(datum_);
				var defaultAttrs = _List_fromArray(
					[
						$terezka$elm_charts$Chart$Attributes$color(interConfig.color),
						$terezka$elm_charts$Chart$Attributes$border(interConfig.color),
						_Utils_eq(interConfig.method, $elm$core$Maybe$Nothing) ? $terezka$elm_charts$Chart$Attributes$circle : $elm$core$Basics$identity
					]);
				var dotAttrs = _Utils_ap(
					defaultAttrs,
					_Utils_ap(
						prop.attrs,
						A5(prop.extra, lineIndex, stackIndex, dataIndex, prop.meta, datum_)));
				var config = toDotConfig(dotAttrs);
				return $terezka$elm_charts$Internal$Item$Rendered(
					{
						config: {
							product: config,
							toAny: $terezka$elm_charts$Internal$Item$Dot,
							tooltipInfo: {
								border: config.border,
								borderWidth: config.borderWidth,
								color: function () {
									var _v6 = config.color;
									if (_v6 === 'white') {
										return interConfig.color;
									} else {
										return config.color;
									}
								}(),
								data: dataIndex,
								elIndex: elIndex,
								formatted: prop.format(datum_),
								index: colorIndex,
								name: prop.meta,
								property: lineIndex,
								stack: stackIndex
							},
							values: {
								datum: datum_,
								isReal: function () {
									var _v7 = prop.value(datum_);
									if (_v7.$ === 'Just') {
										return true;
									} else {
										return false;
									}
								}(),
								x1: x_,
								x2: x_,
								y: y_
							}
						},
						toHtml: function (c) {
							return _List_fromArray(
								[
									A3(
									$terezka$elm_charts$Internal$Produce$tooltipRow,
									c.tooltipInfo.color,
									A2($terezka$elm_charts$Internal$Produce$toDefaultName, colorIndex, c.tooltipInfo.name),
									prop.format(datum_))
								]);
						},
						toLimits: function (_v8) {
							return {x1: x_, x2: x_, y1: y_, y2: y_};
						},
						toPosition: F2(
							function (plane, _v9) {
								var radius = A2(
									$elm$core$Maybe$withDefault,
									0,
									A2(
										$elm$core$Maybe$map,
										$terezka$elm_charts$Internal$Svg$toRadius(config.size),
										config.shape));
								var radiusX_ = A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, radius);
								var radiusY_ = A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, radius);
								return {x1: x_ - radiusX_, x2: x_ + radiusX_, y1: y_ - radiusY_, y2: y_ + radiusY_};
							}),
						toSvg: F3(
							function (plane, _v10, _v11) {
								var _v12 = prop.value(datum_);
								if (_v12.$ === 'Nothing') {
									return $elm$svg$Svg$text('');
								} else {
									return A5(
										$terezka$elm_charts$Internal$Svg$dot,
										plane,
										function ($) {
											return $.x;
										},
										function ($) {
											return $.y;
										},
										config,
										{x: x_, y: y_});
								}
							})
					});
			});
		var toSeriesItem = F5(
			function (lineIndex, stacks, stackIndex, prop, colorIndex) {
				var defaultOpacity = ($elm$core$List$length(stacks) > 1) ? 0.4 : 0;
				var interAttr = _Utils_ap(
					_List_fromArray(
						[
							$terezka$elm_charts$Chart$Attributes$color(
							$terezka$elm_charts$Internal$Helpers$toDefaultColor(colorIndex)),
							$terezka$elm_charts$Chart$Attributes$opacity(defaultOpacity)
						]),
					prop.inter);
				var interConfig = toInterConfig(interAttr);
				var dotItems = A2(
					$elm$core$List$indexedMap,
					A5(toDotItem, lineIndex, stackIndex, colorIndex, prop, interConfig),
					data);
				if (!dotItems.b) {
					return $elm$core$Maybe$Nothing;
				} else {
					var first = dotItems.a;
					var rest = dotItems.b;
					return $elm$core$Maybe$Just(
						$terezka$elm_charts$Internal$Item$Rendered(
							{
								config: {
									items: _Utils_Tuple2(first, rest)
								},
								toHtml: function (c) {
									return _List_fromArray(
										[
											A2(
											$elm$html$Html$table,
											_List_fromArray(
												[
													A2($elm$html$Html$Attributes$style, 'margin', '0')
												]),
											A2(
												$elm$core$List$concatMap,
												$terezka$elm_charts$Internal$Item$toHtml,
												function (_v1) {
													var x = _v1.a;
													var xs = _v1.b;
													return A2($elm$core$List$cons, x, xs);
												}(c.items)))
										]);
								},
								toLimits: function (c) {
									return A2(
										$terezka$elm_charts$Internal$Coordinates$foldPosition,
										$terezka$elm_charts$Internal$Item$getLimits,
										function (_v2) {
											var x = _v2.a;
											var xs = _v2.b;
											return A2($elm$core$List$cons, x, xs);
										}(c.items));
								},
								toPosition: F2(
									function (plane, c) {
										return A2(
											$terezka$elm_charts$Internal$Coordinates$foldPosition,
											$terezka$elm_charts$Internal$Item$getPosition(plane),
											function (_v3) {
												var x = _v3.a;
												var xs = _v3.b;
												return A2($elm$core$List$cons, x, xs);
											}(c.items));
									}),
								toSvg: F3(
									function (plane, _v4, _v5) {
										var toBottom = function (datum_) {
											return A3(
												$elm$core$Maybe$map2,
												F2(
													function (real, visual) {
														return visual - real;
													}),
												prop.value(datum_),
												prop.visual(datum_));
										};
										return A2(
											$elm$svg$Svg$g,
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$class('elm-charts__series')
												]),
											_List_fromArray(
												[
													A6(
													$terezka$elm_charts$Internal$Svg$area,
													plane,
													toX,
													$elm$core$Maybe$Just(toBottom),
													prop.visual,
													interConfig,
													data),
													A5($terezka$elm_charts$Internal$Svg$interpolation, plane, toX, prop.visual, interConfig, data),
													A2(
													$elm$svg$Svg$g,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$class('elm-charts__dots')
														]),
													A2(
														$elm$core$List$map,
														$terezka$elm_charts$Internal$Item$toSvg(plane),
														dotItems))
												]));
									})
							}));
				}
			});
		return A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			A2(
				$elm$core$List$indexedMap,
				F2(
					function (propIndex, f) {
						return f(elIndex + propIndex);
					}),
				$elm$core$List$concat(
					A2(
						$elm$core$List$indexedMap,
						F2(
							function (lineIndex, stacks) {
								return A2(
									$elm$core$List$indexedMap,
									A2(toSeriesItem, lineIndex, stacks),
									stacks);
							}),
						A2($elm$core$List$map, $terezka$elm_charts$Internal$Property$toConfigs, properties)))));
	});
var $terezka$elm_charts$Chart$seriesMap = F4(
	function (mapData, toX, properties, data) {
		return $terezka$elm_charts$Chart$Indexed(
			function (index) {
				var legends_ = A2($terezka$elm_charts$Internal$Legend$toDotLegends, index, properties);
				var items = A4($terezka$elm_charts$Internal$Produce$toDotSeries, index, toX, properties, data);
				var toLimits = A2($elm$core$List$map, $terezka$elm_charts$Internal$Item$getLimits, items);
				var generalized = A2(
					$elm$core$List$map,
					$terezka$elm_charts$Internal$Item$map(mapData),
					A2($elm$core$List$concatMap, $terezka$elm_charts$Internal$Many$getGenerals, items));
				return _Utils_Tuple2(
					A4(
						$terezka$elm_charts$Chart$SeriesElement,
						toLimits,
						generalized,
						legends_,
						function (p) {
							return A2(
								$elm$svg$Svg$map,
								$elm$core$Basics$never,
								A2(
									$elm$svg$Svg$g,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$class('elm-charts__dot-series')
										]),
									A2(
										$elm$core$List$map,
										$terezka$elm_charts$Internal$Item$toSvg(p),
										items)));
						}),
					index + $elm$core$List$length(
						A2($elm$core$List$concatMap, $terezka$elm_charts$Internal$Property$toConfigs, properties)));
			});
	});
var $terezka$elm_charts$Chart$series = F3(
	function (toX, properties, data) {
		return A4($terezka$elm_charts$Chart$seriesMap, $elm$core$Basics$identity, toX, properties, data);
	});
var $author$project$DataConvert$fe1 = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.e1);
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.e1);
		default:
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.e1);
	}
};
var $author$project$DataConvert$fr1 = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.r1);
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.r1);
		default:
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.r1);
	}
};
var $author$project$DataConvert$ft = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.t);
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.t);
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.t);
		default:
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.t);
	}
};
var $author$project$DataConvert$fu1 = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.u1);
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.u1);
		default:
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.u1);
	}
};
var $author$project$DataConvert$fu2 = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.u2);
		default:
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.u2);
	}
};
var $author$project$DataConvert$fu3 = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.u3);
		default:
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.u3);
	}
};
var $author$project$DataConvert$fu4 = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.u4);
		default:
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.u4);
	}
};
var $author$project$DataConvert$fx1 = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.x1);
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.x1);
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.x1);
		default:
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.x1);
	}
};
var $author$project$DataConvert$fx2 = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return $elm$core$Maybe$Nothing;
		default:
			var datum = chartDatum.a;
			return $elm$core$Maybe$Just(datum.x2);
	}
};
var $author$project$DataConvert$stringToAxisFunc = function (str) {
	switch (str) {
		case 't':
			return $elm$core$Maybe$Just($author$project$DataConvert$ft);
		case 'x1':
			return $elm$core$Maybe$Just($author$project$DataConvert$fx1);
		case 'x2':
			return $elm$core$Maybe$Just($author$project$DataConvert$fx2);
		case 'e1':
			return $elm$core$Maybe$Just($author$project$DataConvert$fe1);
		case 'r1':
			return $elm$core$Maybe$Just($author$project$DataConvert$fr1);
		case 'u1':
			return $elm$core$Maybe$Just($author$project$DataConvert$fu1);
		case 'u2':
			return $elm$core$Maybe$Just($author$project$DataConvert$fu2);
		case 'u3':
			return $elm$core$Maybe$Just($author$project$DataConvert$fu3);
		case 'u4':
			return $elm$core$Maybe$Just($author$project$DataConvert$fu4);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $author$project$MyCharts$Chart$Curve$curveToChartSeries = F3(
	function (chartDatum, chartData, model) {
		var _v0 = function ($) {
			return $.axesString;
		}(model);
		var xstr = _v0.a;
		var ystr = _v0.b;
		var maybeXfuncMaybeFloat = $author$project$DataConvert$stringToAxisFunc(xstr);
		var maybeYfuncMaybeFloat = $author$project$DataConvert$stringToAxisFunc(ystr);
		var name = A2($author$project$MyCharts$Chart$Curve$optionToNameCurve, chartDatum, xstr) + (' – ' + A2($author$project$MyCharts$Chart$Curve$optionToNameCurve, chartDatum, ystr));
		var _v1 = _Utils_Tuple2(maybeXfuncMaybeFloat, maybeYfuncMaybeFloat);
		if ((_v1.a.$ === 'Just') && (_v1.b.$ === 'Just')) {
			var xfuncMaybeFloat = _v1.a.a;
			var yfuncMaybeFloat = _v1.b.a;
			var maybeYfunc = A2($author$project$DataConvert$funcMaybeToMaybeFunc, chartDatum, yfuncMaybeFloat);
			var maybeXfunc = A2($author$project$DataConvert$funcMaybeToMaybeFunc, chartDatum, xfuncMaybeFloat);
			var _v2 = _Utils_Tuple2(maybeXfunc, maybeYfunc);
			if ((_v2.a.$ === 'Just') && (_v2.b.$ === 'Just')) {
				var xfunc = _v2.a.a;
				var yfunc = _v2.b.a;
				return $elm$core$Maybe$Just(
					A3(
						$terezka$elm_charts$Chart$series,
						xfunc,
						_List_fromArray(
							[
								A2(
								$terezka$elm_charts$Chart$named,
								name,
								A3(
									$terezka$elm_charts$Chart$interpolated,
									yfunc,
									_List_fromArray(
										[
											$terezka$elm_charts$Chart$Attributes$width(2)
										]),
									_List_Nil))
							]),
						chartData));
			} else {
				return $elm$core$Maybe$Nothing;
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $gampleman$elm_visualization$Zoom$DoubleClicked = F2(
	function (a, b) {
		return {$: 'DoubleClicked', a: a, b: b};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var $elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var $elm$svg$Svg$Events$custom = $elm$html$Html$Events$custom;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $gampleman$elm_visualization$Events$Rect = F4(
	function (x, y, width, height) {
		return {height: height, width: width, x: x, y: y};
	});
var $gampleman$elm_visualization$Events$decodeRect = A5(
	$elm$json$Json$Decode$map4,
	$gampleman$elm_visualization$Events$Rect,
	A2($elm$json$Json$Decode$field, 'x', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'y', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$float));
var $gampleman$elm_visualization$Events$decodeSVGTransformMatrix = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A4(
			$elm$json$Json$Decode$map3,
			F3(
				function (viewBox, width, height) {
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							_Utils_Tuple3(viewBox.width / width, 0, 0),
							_Utils_Tuple3(0, viewBox.height / height, 0)));
				}),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['currentTarget', 'viewBox', 'baseVal']),
				$gampleman$elm_visualization$Events$decodeRect),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['currentTarget', 'width', 'baseVal', 'value']),
				$elm$json$Json$Decode$float),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['currentTarget', 'height', 'baseVal', 'value']),
				$elm$json$Json$Decode$float)),
			$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
		]));
var $gampleman$elm_visualization$Events$decodeMousePosition = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (maybeMatrix, x, y) {
			return A2(
				$gampleman$elm_visualization$Events$normalizePointerPosition,
				_Utils_Tuple2(x, y),
				maybeMatrix);
		}),
	$gampleman$elm_visualization$Events$decodeSVGTransformMatrix,
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float)
			])),
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float)
			])));
var $gampleman$elm_visualization$Zoom$onDoubleClick = F2(
	function (_v0, tagger) {
		return A2(
			$elm$svg$Svg$Events$custom,
			'dblclick',
			A3(
				$elm$json$Json$Decode$map2,
				F2(
					function (shiftDown, position) {
						return {
							message: tagger(
								A2($gampleman$elm_visualization$Zoom$DoubleClicked, shiftDown, position)),
							preventDefault: true,
							stopPropagation: true
						};
					}),
				A2($elm$json$Json$Decode$field, 'shiftKey', $elm$json$Json$Decode$bool),
				$gampleman$elm_visualization$Events$decodeMousePosition));
	});
var $gampleman$elm_visualization$Zoom$MouseDown = F2(
	function (a, b) {
		return {$: 'MouseDown', a: a, b: b};
	});
var $gampleman$elm_visualization$Zoom$onDrag = F2(
	function (_v0, tagger) {
		var drag = _v0.a.drag;
		if (drag.$ === 'Nothing') {
			return _List_fromArray(
				[
					A2(
					$elm$svg$Svg$Events$custom,
					'mousedown',
					A4(
						$elm$json$Json$Decode$map3,
						F3(
							function (x, y, matrix) {
								return {
									message: tagger(
										A2(
											$gampleman$elm_visualization$Zoom$MouseDown,
											_Utils_Tuple2(x, y),
											matrix)),
									preventDefault: false,
									stopPropagation: true
								};
							}),
						A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
						A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float),
						$gampleman$elm_visualization$Events$decodeSVGTransformMatrix))
				]);
		} else {
			return _List_Nil;
		}
	});
var $gampleman$elm_visualization$Zoom$GestureStarted = {$: 'GestureStarted'};
var $gampleman$elm_visualization$Zoom$Gestured = F2(
	function (a, b) {
		return {$: 'Gestured', a: a, b: b};
	});
var $gampleman$elm_visualization$Zoom$onGesture = F2(
	function (_v0, tagger) {
		return _List_fromArray(
			[
				A2(
				$elm$svg$Svg$Events$custom,
				'gesturestart',
				$elm$json$Json$Decode$succeed(
					{
						message: tagger($gampleman$elm_visualization$Zoom$GestureStarted),
						preventDefault: true,
						stopPropagation: true
					})),
				A2(
				$elm$svg$Svg$Events$custom,
				'gesturechange',
				A3(
					$elm$json$Json$Decode$map2,
					F2(
						function (scl, position) {
							return {
								message: tagger(
									A2($gampleman$elm_visualization$Zoom$Gestured, scl, position)),
								preventDefault: true,
								stopPropagation: true
							};
						}),
					A2($elm$json$Json$Decode$field, 'scale', $elm$json$Json$Decode$float),
					$gampleman$elm_visualization$Events$decodeMousePosition))
			]);
	});
var $gampleman$elm_visualization$Zoom$TouchEnded = function (a) {
	return {$: 'TouchEnded', a: a};
};
var $gampleman$elm_visualization$Zoom$TouchMoved = function (a) {
	return {$: 'TouchMoved', a: a};
};
var $gampleman$elm_visualization$Zoom$TouchStarted = function (a) {
	return {$: 'TouchStarted', a: a};
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $gampleman$elm_visualization$Events$listLike = function (itemDecoder) {
	var decodeOne = function (n) {
		return A2(
			$elm$json$Json$Decode$field,
			$elm$core$String$fromInt(n),
			itemDecoder);
	};
	var decodeN = function (n) {
		return A3(
			$elm$core$List$foldr,
			$elm$json$Json$Decode$map2($elm$core$List$cons),
			$elm$json$Json$Decode$succeed(_List_Nil),
			A2(
				$elm$core$List$map,
				decodeOne,
				A2($elm$core$List$range, 0, n - 1)));
	};
	return A2(
		$elm$json$Json$Decode$andThen,
		decodeN,
		A2($elm$json$Json$Decode$field, 'length', $elm$json$Json$Decode$int));
};
var $gampleman$elm_visualization$Events$decodeTouches = A2(
	$elm$json$Json$Decode$andThen,
	function (maybeMatrix) {
		return A2(
			$elm$json$Json$Decode$field,
			'changedTouches',
			$gampleman$elm_visualization$Events$listLike(
				A4(
					$elm$json$Json$Decode$map3,
					F3(
						function (x, y, identifier) {
							return {
								identifier: identifier,
								position: A2(
									$gampleman$elm_visualization$Events$normalizePointerPosition,
									_Utils_Tuple2(x, y),
									maybeMatrix)
							};
						}),
					A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float),
					A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float),
					A2($elm$json$Json$Decode$field, 'identifier', $elm$json$Json$Decode$int))));
	},
	$gampleman$elm_visualization$Events$decodeSVGTransformMatrix);
var $gampleman$elm_visualization$Zoom$onTouch = F2(
	function (_v0, tagger) {
		var zoom = _v0.a;
		return _List_fromArray(
			[
				A2(
				$elm$svg$Svg$Events$custom,
				'touchstart',
				A2(
					$elm$json$Json$Decode$map,
					function (touches) {
						var _v1 = _Utils_Tuple3(zoom.touchStarted, touches, zoom.touches);
						if (((_v1.a && _v1.b.b) && (!_v1.b.b.b)) && (_v1.c.$ === 'NoTouches')) {
							var _v2 = _v1.b;
							var touch0 = _v2.a;
							var _v3 = _v1.c;
							return {
								message: tagger(
									A2($gampleman$elm_visualization$Zoom$DoubleClicked, false, touch0.position)),
								preventDefault: true,
								stopPropagation: true
							};
						} else {
							return {
								message: tagger(
									$gampleman$elm_visualization$Zoom$TouchStarted(touches)),
								preventDefault: false,
								stopPropagation: true
							};
						}
					},
					$gampleman$elm_visualization$Events$decodeTouches)),
				A2(
				$elm$svg$Svg$Events$custom,
				'touchmove',
				A2(
					$elm$json$Json$Decode$map,
					function (touches) {
						return {
							message: tagger(
								$gampleman$elm_visualization$Zoom$TouchMoved(touches)),
							preventDefault: true,
							stopPropagation: true
						};
					},
					$gampleman$elm_visualization$Events$decodeTouches)),
				A2(
				$elm$svg$Svg$Events$custom,
				'touchend',
				A2(
					$elm$json$Json$Decode$map,
					function (touches) {
						return {
							message: tagger(
								$gampleman$elm_visualization$Zoom$TouchEnded(touches)),
							preventDefault: false,
							stopPropagation: true
						};
					},
					$gampleman$elm_visualization$Events$decodeTouches)),
				A2(
				$elm$svg$Svg$Events$custom,
				'touchcancel',
				A2(
					$elm$json$Json$Decode$map,
					function (touches) {
						return {
							message: tagger(
								$gampleman$elm_visualization$Zoom$TouchEnded(touches)),
							preventDefault: false,
							stopPropagation: true
						};
					},
					$gampleman$elm_visualization$Events$decodeTouches)),
				A2($elm$html$Html$Attributes$style, 'touch-action', 'none'),
				A2($elm$html$Html$Attributes$style, '-webkit-tap-highlight-color', 'rgba(0,0,0,0)')
			]);
	});
var $gampleman$elm_visualization$Zoom$Wheeled = F2(
	function (a, b) {
		return {$: 'Wheeled', a: a, b: b};
	});
var $gampleman$elm_visualization$Zoom$onWheel = F2(
	function (_v0, tagger) {
		return A2(
			$elm$svg$Svg$Events$custom,
			'wheel',
			A4(
				$elm$json$Json$Decode$map3,
				F3(
					function (deltaY, deltaMode, position) {
						return {
							message: tagger(
								A2(
									$gampleman$elm_visualization$Zoom$Wheeled,
									(-deltaY) * ((!deltaMode) ? 0.002 : ((deltaMode === 1) ? 0.05 : 1)),
									position)),
							preventDefault: true,
							stopPropagation: true
						};
					}),
				A2($elm$json$Json$Decode$field, 'deltaY', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'deltaMode', $elm$json$Json$Decode$int),
				$gampleman$elm_visualization$Events$decodeMousePosition));
	});
var $gampleman$elm_visualization$Zoom$events = F2(
	function (zoom, tagger) {
		return A2(
			$elm$core$List$cons,
			A2($gampleman$elm_visualization$Zoom$onDoubleClick, zoom, tagger),
			A2(
				$elm$core$List$cons,
				A2($gampleman$elm_visualization$Zoom$onWheel, zoom, tagger),
				_Utils_ap(
					A2($gampleman$elm_visualization$Zoom$onDrag, zoom, tagger),
					_Utils_ap(
						A2($gampleman$elm_visualization$Zoom$onGesture, zoom, tagger),
						A2($gampleman$elm_visualization$Zoom$onTouch, zoom, tagger)))));
	});
var $terezka$elm_charts$Chart$Attributes$height = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{height: v});
	});
var $terezka$elm_charts$Chart$HtmlElement = function (a) {
	return {$: 'HtmlElement', a: a};
};
var $terezka$elm_charts$Internal$Coordinates$Axis = F4(
	function (dataMin, dataMax, min, max) {
		return {dataMax: dataMax, dataMin: dataMin, max: max, min: min};
	});
var $terezka$elm_charts$Internal$Coordinates$Margin = F4(
	function (top, right, left, bottom) {
		return {bottom: bottom, left: left, right: right, top: top};
	});
var $terezka$elm_charts$Internal$Svg$bar = F3(
	function (plane, config, point) {
		var viewBar = F7(
			function (fill, fillOpacity, border, borderWidth, strokeOpacity, cmds, limits) {
				return A4(
					$terezka$elm_charts$Internal$Svg$withAttrs,
					config.attrs,
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('elm-charts__bar'),
							$elm$svg$Svg$Attributes$fill(fill),
							$elm$svg$Svg$Attributes$fillOpacity(
							$elm$core$String$fromFloat(fillOpacity)),
							$elm$svg$Svg$Attributes$stroke(border),
							$elm$svg$Svg$Attributes$strokeWidth(
							$elm$core$String$fromFloat(borderWidth)),
							$elm$svg$Svg$Attributes$strokeOpacity(
							$elm$core$String$fromFloat(strokeOpacity)),
							$elm$svg$Svg$Attributes$d(
							A2($terezka$elm_charts$Internal$Commands$description, plane, cmds)),
							$elm$svg$Svg$Attributes$style(
							A2($terezka$elm_charts$Internal$Svg$clipperStyle, plane, limits))
						]),
					_List_Nil);
			});
		var highlightColor = (config.highlightColor === '') ? config.color : config.highlightColor;
		var borderWidthCarY = A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, config.borderWidth / 2);
		var highlightWidthCarY = borderWidthCarY + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, config.highlightWidth / 2);
		var borderWidthCarX = A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, config.borderWidth / 2);
		var highlightWidthCarX = borderWidthCarX + A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, config.highlightWidth / 2);
		var pos = {
			x1: A2($elm$core$Basics$min, point.x1, point.x2) + borderWidthCarX,
			x2: A2($elm$core$Basics$max, point.x1, point.x2) - borderWidthCarX,
			y1: A2($elm$core$Basics$min, point.y1, point.y2) + borderWidthCarY,
			y2: A2($elm$core$Basics$max, point.y1, point.y2) - borderWidthCarY
		};
		var highlightPos = {x1: pos.x1 - highlightWidthCarX, x2: pos.x2 + highlightWidthCarX, y1: pos.y1 - highlightWidthCarY, y2: pos.y2 + highlightWidthCarY};
		var w = $elm$core$Basics$abs(pos.x2 - pos.x1);
		var roundingBottom = (A2($terezka$elm_charts$Internal$Coordinates$scaleSVGX, plane, w) * 0.5) * A3($elm$core$Basics$clamp, 0, 1, config.roundBottom);
		var radiusBottomX = A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, roundingBottom);
		var radiusBottomY = A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, roundingBottom);
		var roundingTop = (A2($terezka$elm_charts$Internal$Coordinates$scaleSVGX, plane, w) * 0.5) * A3($elm$core$Basics$clamp, 0, 1, config.roundTop);
		var radiusTopX = A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianX, plane, roundingTop);
		var radiusTopY = A2($terezka$elm_charts$Internal$Coordinates$scaleCartesianY, plane, roundingTop);
		var _v0 = function () {
			if (_Utils_eq(pos.y1, pos.y2)) {
				return _Utils_Tuple3(_List_Nil, _List_Nil, highlightPos);
			} else {
				var _v1 = _Utils_Tuple2(config.roundTop > 0, config.roundBottom > 0);
				if (!_v1.a) {
					if (!_v1.b) {
						return _Utils_Tuple3(
							_List_fromArray(
								[
									A2($terezka$elm_charts$Internal$Commands$Move, pos.x1, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y1)
								]),
							_List_fromArray(
								[
									A2($terezka$elm_charts$Internal$Commands$Move, highlightPos.x1, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x1, highlightPos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x2, highlightPos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x2, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y1)
								]),
							{x1: pos.x1 - highlightWidthCarX, x2: pos.x2 + highlightWidthCarX, y1: pos.y1, y2: pos.y2 + highlightWidthCarY});
					} else {
						return _Utils_Tuple3(
							_List_fromArray(
								[
									A2($terezka$elm_charts$Internal$Commands$Move, pos.x1 + radiusBottomX, pos.y1),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingBottom, roundingBottom, -45, false, true, pos.x1, pos.y1 + radiusBottomY),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y1 + radiusBottomY),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingBottom, roundingBottom, -45, false, true, pos.x2 - radiusBottomX, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1 + radiusBottomX, pos.y1)
								]),
							_List_fromArray(
								[
									A2($terezka$elm_charts$Internal$Commands$Move, highlightPos.x1 + radiusBottomX, highlightPos.y1),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingBottom, roundingBottom, -45, false, true, highlightPos.x1, highlightPos.y1 + radiusBottomY),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x1, highlightPos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x2, highlightPos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x2, highlightPos.y1 + radiusBottomY),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingBottom, roundingBottom, -45, false, true, highlightPos.x2 - radiusBottomX, highlightPos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x1 + radiusBottomX, highlightPos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2 - radiusBottomX, pos.y1),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingBottom, roundingBottom, -45, false, false, pos.x2, pos.y1 + radiusBottomY),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y1 + radiusBottomY),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y1)
								]),
							{x1: pos.x1 - highlightWidthCarX, x2: pos.x2 + highlightWidthCarX, y1: pos.y1 - highlightWidthCarY, y2: pos.y2 + highlightWidthCarY});
					}
				} else {
					if (!_v1.b) {
						return _Utils_Tuple3(
							_List_fromArray(
								[
									A2($terezka$elm_charts$Internal$Commands$Move, pos.x1, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y2 - radiusTopY),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, true, pos.x1 + radiusTopX, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2 - radiusTopX, pos.y2),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, true, pos.x2, pos.y2 - radiusTopY),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y1)
								]),
							_List_fromArray(
								[
									A2($terezka$elm_charts$Internal$Commands$Move, highlightPos.x1, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x1, highlightPos.y2 - radiusTopY),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, true, highlightPos.x1 + radiusTopX, highlightPos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x2 - radiusTopX, highlightPos.y2),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, true, highlightPos.x2, highlightPos.y2 - radiusTopY),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x2, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y2 - radiusTopY),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, false, pos.x2 - radiusTopX, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1 + radiusTopX, pos.y2),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, false, pos.x1, pos.y2 - radiusTopY),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y1)
								]),
							{x1: pos.x1 - highlightWidthCarX, x2: pos.x2 + highlightWidthCarX, y1: pos.y1, y2: pos.y2 + highlightWidthCarY});
					} else {
						return _Utils_Tuple3(
							_List_fromArray(
								[
									A2($terezka$elm_charts$Internal$Commands$Move, pos.x1 + radiusBottomX, pos.y1),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingBottom, roundingBottom, -45, false, true, pos.x1, pos.y1 + radiusBottomY),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y2 - radiusTopY),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, true, pos.x1 + radiusTopX, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2 - radiusTopX, pos.y2),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, true, pos.x2, pos.y2 - radiusTopY),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y1 + radiusBottomY),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingBottom, roundingBottom, -45, false, true, pos.x2 - radiusBottomX, pos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1 + radiusBottomX, pos.y1)
								]),
							_List_fromArray(
								[
									A2($terezka$elm_charts$Internal$Commands$Move, highlightPos.x1 + radiusBottomX, highlightPos.y1),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingBottom, roundingBottom, -45, false, true, highlightPos.x1, highlightPos.y1 + radiusBottomY),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x1, highlightPos.y2 - radiusTopY),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, true, highlightPos.x1 + radiusTopX, highlightPos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x2 - radiusTopX, highlightPos.y2),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, true, highlightPos.x2, highlightPos.y2 - radiusTopY),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x2, highlightPos.y1 + radiusBottomY),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingBottom, roundingBottom, -45, false, true, highlightPos.x2 - radiusBottomX, highlightPos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, highlightPos.x1 + radiusBottomX, highlightPos.y1),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2 - radiusBottomX, pos.y1),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingBottom, roundingBottom, -45, false, false, pos.x2, pos.y1 + radiusBottomY),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y2 - radiusTopY),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, false, pos.x2 - radiusTopX, pos.y2),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1 + radiusTopX, pos.y2),
									A7($terezka$elm_charts$Internal$Commands$Arc, roundingTop, roundingTop, -45, false, false, pos.x1, pos.y2 - radiusTopY),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x1, pos.y1 + radiusBottomY),
									A2($terezka$elm_charts$Internal$Commands$Line, pos.x2, pos.y1)
								]),
							{x1: pos.x1 - highlightWidthCarX, x2: pos.x2 + highlightWidthCarX, y1: pos.y1 - highlightWidthCarY, y2: pos.y2 + highlightWidthCarY});
					}
				}
			}
		}();
		var commands = _v0.a;
		var highlightCommands = _v0.b;
		var highlightCut = _v0.c;
		var viewAuraBar = function (fill) {
			return (!config.highlight) ? A7(viewBar, fill, config.opacity, config.border, config.borderWidth, 1, commands, pos) : A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('elm-charts__bar-with-highlight')
					]),
				_List_fromArray(
					[
						A7(viewBar, highlightColor, config.highlight, 'transparent', 0, 0, highlightCommands, highlightCut),
						A7(viewBar, fill, config.opacity, config.border, config.borderWidth, 1, commands, pos)
					]));
		};
		var _v2 = config.design;
		if (_v2.$ === 'Nothing') {
			return viewAuraBar(config.color);
		} else {
			var design = _v2.a;
			var _v3 = A2($terezka$elm_charts$Internal$Svg$toPattern, config.color, design);
			var patternDefs = _v3.a;
			var fill = _v3.b;
			return A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('elm-charts__bar-with-pattern')
					]),
				_List_fromArray(
					[
						patternDefs,
						viewAuraBar(fill)
					]));
		}
	});
var $terezka$elm_charts$Internal$Svg$defaultContainer = {
	attrs: _List_fromArray(
		[
			$elm$svg$Svg$Attributes$style('overflow: visible;')
		]),
	events: _List_Nil,
	htmlAttrs: _List_Nil,
	responsive: true
};
var $terezka$elm_charts$Internal$Svg$barLegend = F2(
	function (config, barConfig) {
		var fontStyle = function () {
			var _v0 = config.fontSize;
			if (_v0.$ === 'Just') {
				var size_ = _v0.a;
				return A2(
					$elm$html$Html$Attributes$style,
					'font-size',
					$elm$core$String$fromInt(size_) + 'px');
			} else {
				return A2($elm$html$Html$Attributes$style, '', '');
			}
		}();
		var fakePlane = {
			height: config.height,
			margin: A4($terezka$elm_charts$Internal$Coordinates$Margin, 0, 0, 0, 0),
			width: config.width,
			x: A4($terezka$elm_charts$Internal$Coordinates$Axis, 0, 10, 0, 10),
			y: A4($terezka$elm_charts$Internal$Coordinates$Axis, 0, 10, 0, 10)
		};
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('elm-charts__legend'),
						A2($elm$html$Html$Attributes$style, 'display', 'flex'),
						A2($elm$html$Html$Attributes$style, 'align-items', 'center')
					]),
				config.htmlAttrs),
			_List_fromArray(
				[
					A5(
					$terezka$elm_charts$Internal$Svg$container,
					fakePlane,
					_Utils_update(
						$terezka$elm_charts$Internal$Svg$defaultContainer,
						{responsive: false}),
					_List_Nil,
					_List_fromArray(
						[
							A3(
							$terezka$elm_charts$Internal$Svg$bar,
							fakePlane,
							barConfig,
							{x1: 0, x2: 10, y1: 0, y2: 10})
						]),
					_List_Nil),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							fontStyle,
							A2(
							$elm$html$Html$Attributes$style,
							'margin-left',
							$elm$core$String$fromFloat(config.spacing) + 'px')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(config.title)
						]))
				]));
	});
var $terezka$elm_charts$Internal$Svg$defaultBar = {attrs: _List_Nil, border: 'white', borderWidth: 0, color: $terezka$elm_charts$Internal$Helpers$pink, design: $elm$core$Maybe$Nothing, highlight: 0, highlightColor: '', highlightWidth: 10, opacity: 1, roundBottom: 0, roundTop: 0};
var $terezka$elm_charts$Internal$Svg$defaultBarLegend = {color: '#808BAB', fontSize: $elm$core$Maybe$Nothing, height: 10, htmlAttrs: _List_Nil, spacing: 10, title: '', width: 10, xOff: 0, yOff: 0};
var $terezka$elm_charts$Chart$Svg$barLegend = F2(
	function (edits, barAttrs) {
		return A2(
			$terezka$elm_charts$Internal$Svg$barLegend,
			A2($terezka$elm_charts$Internal$Helpers$apply, edits, $terezka$elm_charts$Internal$Svg$defaultBarLegend),
			A2($terezka$elm_charts$Internal$Helpers$apply, barAttrs, $terezka$elm_charts$Internal$Svg$defaultBar));
	});
var $terezka$elm_charts$Internal$Svg$Row = {$: 'Row'};
var $terezka$elm_charts$Internal$Svg$defaultLegends = {alignment: $terezka$elm_charts$Internal$Svg$Row, anchor: $elm$core$Maybe$Nothing, background: '', border: '', borderWidth: 0, htmlAttrs: _List_Nil, spacing: 10, xOff: 0, yOff: 0};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $terezka$elm_charts$Internal$Svg$positionHtml = F7(
	function (plane, x, y, xOff, yOff, attrs, content) {
		var yPercentage = ((A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, y) - yOff) * 100) / plane.height;
		var xPercentage = ((A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, x) + xOff) * 100) / plane.width;
		var posititonStyles = _List_fromArray(
			[
				A2(
				$elm$html$Html$Attributes$style,
				'left',
				$elm$core$String$fromFloat(xPercentage) + '%'),
				A2(
				$elm$html$Html$Attributes$style,
				'top',
				$elm$core$String$fromFloat(yPercentage) + '%'),
				A2($elm$html$Html$Attributes$style, 'margin-right', '-400px'),
				A2($elm$html$Html$Attributes$style, 'position', 'absolute')
			]);
		return A2(
			$elm$html$Html$div,
			_Utils_ap(posititonStyles, attrs),
			content);
	});
var $terezka$elm_charts$Internal$Svg$legendsAt = F5(
	function (plane, x, y, config, children) {
		var otherAttrs = _List_fromArray(
			[
				$elm$html$Html$Attributes$class('elm-charts__legends'),
				A2($elm$html$Html$Attributes$style, 'background', config.background),
				A2($elm$html$Html$Attributes$style, 'border-color', config.border),
				A2(
				$elm$html$Html$Attributes$style,
				'border-width',
				$elm$core$String$fromFloat(config.borderWidth) + 'px'),
				A2($elm$html$Html$Attributes$style, 'border-style', 'solid')
			]);
		var anchorAttrs = function () {
			var _v2 = config.anchor;
			if (_v2.$ === 'Nothing') {
				return _List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'transform', 'translate(-0%, 0%)')
					]);
			} else {
				switch (_v2.a.$) {
					case 'End':
						var _v3 = _v2.a;
						return _List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'transform', 'translate(-100%, 0%)')
							]);
					case 'Start':
						var _v4 = _v2.a;
						return _List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'transform', 'translate(-0%, 0%)')
							]);
					default:
						var _v5 = _v2.a;
						return _List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'transform', 'translate(-50%, 0%)')
							]);
				}
			}
		}();
		var _v0 = function () {
			var _v1 = config.alignment;
			if (_v1.$ === 'Row') {
				return _Utils_Tuple2(
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'display', 'flex'),
							A2($elm$html$Html$Attributes$style, 'align-items', 'center')
						]),
					'right');
			} else {
				return _Utils_Tuple2(
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'display', 'flex'),
							A2($elm$html$Html$Attributes$style, 'flex-direction', 'column'),
							A2($elm$html$Html$Attributes$style, 'align-items', 'baseline')
						]),
					'bottom');
			}
		}();
		var alignmentAttrs = _v0.a;
		var direction = _v0.b;
		var paddingStyle = ' .elm-charts__legends .elm-charts__legend {\n              margin-' + (direction + (':' + ($elm$core$String$fromFloat(config.spacing) + ('px;\n            }\n\n            .elm-charts__legends .elm-charts__legend:last-child {\n              margin-' + (direction + ': 0px;\n            }\n        ')))));
		return A7(
			$terezka$elm_charts$Internal$Svg$positionHtml,
			plane,
			x,
			y,
			config.xOff,
			-config.yOff,
			_Utils_ap(
				anchorAttrs,
				_Utils_ap(
					alignmentAttrs,
					_Utils_ap(otherAttrs, config.htmlAttrs))),
			A2(
				$elm$core$List$cons,
				A3(
					$elm$html$Html$node,
					'style',
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(paddingStyle)
						])),
				children));
	});
var $terezka$elm_charts$Chart$Svg$legendsAt = F4(
	function (plane, x, y, edits) {
		return A4(
			$terezka$elm_charts$Internal$Svg$legendsAt,
			plane,
			x,
			y,
			A2($terezka$elm_charts$Internal$Helpers$apply, edits, $terezka$elm_charts$Internal$Svg$defaultLegends));
	});
var $terezka$elm_charts$Internal$Svg$defaultLineLegend = {color: '#808BAB', fontSize: $elm$core$Maybe$Nothing, height: 16, htmlAttrs: _List_Nil, spacing: 10, title: '', width: 30, xOff: 0, yOff: 0};
var $terezka$elm_charts$Internal$Svg$Point = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var $terezka$elm_charts$Internal$Svg$lineLegend = F3(
	function (config, interConfig, dotConfig) {
		var topMargin = function () {
			var _v1 = dotConfig.shape;
			if (_v1.$ === 'Just') {
				var shape = _v1.a;
				return A2($terezka$elm_charts$Internal$Svg$toRadius, dotConfig.size, shape);
			} else {
				return 0;
			}
		}();
		var fontStyle = function () {
			var _v0 = config.fontSize;
			if (_v0.$ === 'Just') {
				var size_ = _v0.a;
				return A2(
					$elm$html$Html$Attributes$style,
					'font-size',
					$elm$core$String$fromInt(size_) + 'px');
			} else {
				return A2($elm$html$Html$Attributes$style, '', '');
			}
		}();
		var fakePlane = {
			height: config.height,
			margin: A4($terezka$elm_charts$Internal$Coordinates$Margin, 0, 0, 0, 0),
			width: config.width,
			x: A4($terezka$elm_charts$Internal$Coordinates$Axis, 0, 10, 0, 10),
			y: A4($terezka$elm_charts$Internal$Coordinates$Axis, 0, 10, 0, 10)
		};
		var bottomMargin = (!interConfig.opacity) ? topMargin : 0;
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('elm-charts__legend'),
						A2($elm$html$Html$Attributes$style, 'display', 'flex'),
						A2($elm$html$Html$Attributes$style, 'align-items', 'center')
					]),
				config.htmlAttrs),
			_List_fromArray(
				[
					A5(
					$terezka$elm_charts$Internal$Svg$container,
					fakePlane,
					_Utils_update(
						$terezka$elm_charts$Internal$Svg$defaultContainer,
						{responsive: false}),
					_List_Nil,
					_List_fromArray(
						[
							A5(
							$terezka$elm_charts$Internal$Svg$interpolation,
							fakePlane,
							function ($) {
								return $.x;
							},
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.y;
								},
								$elm$core$Maybe$Just),
							interConfig,
							_List_fromArray(
								[
									A2($terezka$elm_charts$Internal$Svg$Point, 0, 5),
									A2($terezka$elm_charts$Internal$Svg$Point, 10, 5)
								])),
							A6(
							$terezka$elm_charts$Internal$Svg$area,
							fakePlane,
							function ($) {
								return $.x;
							},
							$elm$core$Maybe$Nothing,
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.y;
								},
								$elm$core$Maybe$Just),
							interConfig,
							_List_fromArray(
								[
									A2($terezka$elm_charts$Internal$Svg$Point, 0, 5),
									A2($terezka$elm_charts$Internal$Svg$Point, 10, 5)
								])),
							A5(
							$terezka$elm_charts$Internal$Svg$dot,
							fakePlane,
							function ($) {
								return $.x;
							},
							function ($) {
								return $.y;
							},
							dotConfig,
							A2($terezka$elm_charts$Internal$Svg$Point, 5, 5))
						]),
					_List_Nil),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							fontStyle,
							A2(
							$elm$html$Html$Attributes$style,
							'margin-left',
							$elm$core$String$fromFloat(config.spacing) + 'px')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(config.title)
						]))
				]));
	});
var $terezka$elm_charts$Chart$Svg$lineLegend = F3(
	function (edits, interAttrsOrg, dotAttrsOrg) {
		var interpolationConfigOrg = A2($terezka$elm_charts$Internal$Helpers$apply, interAttrsOrg, $terezka$elm_charts$Internal$Svg$defaultInterpolation);
		var dotConfigOrg = A2($terezka$elm_charts$Internal$Helpers$apply, dotAttrsOrg, $terezka$elm_charts$Internal$Svg$defaultDot);
		var adjustWidth = function (config) {
			return _Utils_update(
				config,
				{width: 10});
		};
		var _v0 = function () {
			var _v1 = _Utils_Tuple2(interpolationConfigOrg.method, dotConfigOrg.shape);
			if (_v1.a.$ === 'Just') {
				if (_v1.b.$ === 'Nothing') {
					var _v2 = _v1.b;
					return _Utils_Tuple3(
						dotAttrsOrg,
						interAttrsOrg,
						A2(
							$elm$core$List$cons,
							$terezka$elm_charts$Chart$Attributes$width(10),
							edits));
				} else {
					return _Utils_Tuple3(
						dotAttrsOrg,
						A2(
							$elm$core$List$cons,
							$terezka$elm_charts$Chart$Attributes$opacity(0),
							interAttrsOrg),
						edits);
				}
			} else {
				if (_v1.b.$ === 'Nothing') {
					var _v3 = _v1.a;
					var _v4 = _v1.b;
					return _Utils_Tuple3(
						A2($elm$core$List$cons, $terezka$elm_charts$Chart$Attributes$circle, dotAttrsOrg),
						A2($elm$core$List$cons, $terezka$elm_charts$Chart$Attributes$linear, interAttrsOrg),
						A2(
							$elm$core$List$cons,
							$terezka$elm_charts$Chart$Attributes$width(10),
							edits));
				} else {
					var _v5 = _v1.a;
					return _Utils_Tuple3(
						A2($elm$core$List$cons, $terezka$elm_charts$Chart$Attributes$circle, dotAttrsOrg),
						interAttrsOrg,
						A2(
							$elm$core$List$cons,
							$terezka$elm_charts$Chart$Attributes$width(10),
							edits));
				}
			}
		}();
		var dotAttrs = _v0.a;
		var interAttrs = _v0.b;
		var lineLegendAttrs = _v0.c;
		return A3(
			$terezka$elm_charts$Internal$Svg$lineLegend,
			A2($terezka$elm_charts$Internal$Helpers$apply, lineLegendAttrs, $terezka$elm_charts$Internal$Svg$defaultLineLegend),
			A2($terezka$elm_charts$Internal$Helpers$apply, interAttrs, $terezka$elm_charts$Internal$Svg$defaultInterpolation),
			A2($terezka$elm_charts$Internal$Helpers$apply, dotAttrs, $terezka$elm_charts$Internal$Svg$defaultDot));
	});
var $terezka$elm_charts$Chart$Attributes$title = F2(
	function (value, config) {
		return _Utils_update(
			config,
			{title: value});
	});
var $terezka$elm_charts$Chart$legendsAt = F4(
	function (toX, toY, attrs, children) {
		return $terezka$elm_charts$Chart$HtmlElement(
			F2(
				function (p, legends_) {
					var viewLegend = function (legend) {
						if (legend.$ === 'BarLegend') {
							var name = legend.a;
							var barAttrs = legend.b;
							return A2(
								$terezka$elm_charts$Chart$Svg$barLegend,
								A2(
									$elm$core$List$cons,
									$terezka$elm_charts$Chart$Attributes$title(name),
									children),
								barAttrs);
						} else {
							var name = legend.a;
							var interAttrs = legend.b;
							var dotAttrs = legend.c;
							return A3(
								$terezka$elm_charts$Chart$Svg$lineLegend,
								A2(
									$elm$core$List$cons,
									$terezka$elm_charts$Chart$Attributes$title(name),
									children),
								interAttrs,
								dotAttrs);
						}
					};
					return A5(
						$terezka$elm_charts$Chart$Svg$legendsAt,
						p,
						toX(p.x),
						toY(p.y),
						attrs,
						A2($elm$core$List$map, viewLegend, legends_));
				}));
	});
var $terezka$elm_charts$Chart$Attributes$margin = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{margin: v});
	});
var $terezka$elm_charts$Chart$Attributes$moveRight = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{xOff: config.xOff + v});
	});
var $terezka$elm_charts$Chart$Attributes$moveUp = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{yOff: config.yOff - v});
	});
var $terezka$elm_charts$Chart$Attributes$padding = F2(
	function (value, config) {
		return _Utils_update(
			config,
			{padding: value});
	});
var $terezka$elm_charts$Chart$Attributes$pinned = F2(
	function (value, config) {
		return _Utils_update(
			config,
			{pinned: value});
	});
var $terezka$elm_charts$Chart$Attributes$row = function (config) {
	return _Utils_update(
		config,
		{alignment: $terezka$elm_charts$Internal$Svg$Row});
};
var $terezka$elm_charts$Chart$Attributes$spacing = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{spacing: v});
	});
var $gampleman$elm_visualization$Zoom$Transform$toString = function (_v0) {
	var k = _v0.k;
	var x = _v0.x;
	var y = _v0.y;
	return 'translate(' + ($elm$core$String$fromFloat(x) + (',' + ($elm$core$String$fromFloat(y) + (') scale(' + ($elm$core$String$fromFloat(k) + ')')))));
};
var $gampleman$elm_visualization$Zoom$transform = function (_v0) {
	var zoom = _v0.a;
	return $elm$svg$Svg$Attributes$transform(
		$gampleman$elm_visualization$Zoom$Transform$toString(zoom.transform));
};
var $terezka$elm_charts$Chart$Attributes$withGrid = function (config) {
	return _Utils_update(
		config,
		{grid: true});
};
var $terezka$elm_charts$Chart$AxisElement = F2(
	function (a, b) {
		return {$: 'AxisElement', a: a, b: b};
	});
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $terezka$elm_charts$Internal$Svg$position = F6(
	function (plane, rotation, x_, y_, xOff_, yOff_) {
		return $elm$svg$Svg$Attributes$transform(
			'translate(' + ($elm$core$String$fromFloat(
				A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, x_) + xOff_) + (',' + ($elm$core$String$fromFloat(
				A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, y_) + yOff_) + (') rotate(' + ($elm$core$String$fromFloat(rotation) + ')'))))));
	});
var $terezka$elm_charts$Internal$Svg$arrow = F3(
	function (plane, config, point) {
		var points_ = '0,0 ' + ($elm$core$String$fromFloat(config.length) + (',' + ($elm$core$String$fromFloat(config.width) + (' 0, ' + $elm$core$String$fromFloat(config.width * 2)))));
		var commands = 'rotate(' + ($elm$core$String$fromFloat(config.rotate) + (') translate(0 ' + ($elm$core$String$fromFloat(-config.width) + ') ')));
		return A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('elm-charts__arrow'),
					A6($terezka$elm_charts$Internal$Svg$position, plane, 0, point.x, point.y, config.xOff, config.yOff)
				]),
			_List_fromArray(
				[
					A4(
					$terezka$elm_charts$Internal$Svg$withAttrs,
					config.attrs,
					$elm$svg$Svg$polygon,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$fill(config.color),
							$elm$svg$Svg$Attributes$points(points_),
							$elm$svg$Svg$Attributes$transform(commands)
						]),
					_List_Nil)
				]));
	});
var $terezka$elm_charts$Internal$Svg$defaultArrow = {attrs: _List_Nil, color: 'rgb(210, 210, 210)', length: 7, rotate: 0, width: 4, xOff: 0, yOff: 0};
var $terezka$elm_charts$Chart$Svg$arrow = F2(
	function (plane, edits) {
		return A2(
			$terezka$elm_charts$Internal$Svg$arrow,
			plane,
			A2($terezka$elm_charts$Internal$Helpers$apply, edits, $terezka$elm_charts$Internal$Svg$defaultArrow));
	});
var $terezka$elm_charts$Chart$Attributes$x2 = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{
				x2: $elm$core$Maybe$Just(v)
			});
	});
var $terezka$elm_charts$Chart$Attributes$zero = function (b) {
	return A3($elm$core$Basics$clamp, b.min, b.max, 0);
};
var $terezka$elm_charts$Chart$xAxis = function (edits) {
	var config = A2(
		$terezka$elm_charts$Internal$Helpers$apply,
		edits,
		{arrow: true, color: '', limits: _List_Nil, pinned: $terezka$elm_charts$Chart$Attributes$zero, width: 1});
	var addTickValues = F2(
		function (p, ts) {
			return _Utils_update(
				ts,
				{
					yAxis: A2(
						$elm$core$List$cons,
						config.pinned(p.y),
						ts.yAxis)
				});
		});
	return A2(
		$terezka$elm_charts$Chart$AxisElement,
		addTickValues,
		function (p) {
			var xLimit = A3(
				$elm$core$List$foldl,
				F2(
					function (f, x) {
						return f(x);
					}),
				p.x,
				config.limits);
			return A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('elm-charts__x-axis')
					]),
				_List_fromArray(
					[
						A2(
						$terezka$elm_charts$Chart$Svg$line,
						p,
						_List_fromArray(
							[
								$terezka$elm_charts$Chart$Attributes$color(config.color),
								$terezka$elm_charts$Chart$Attributes$width(config.width),
								$terezka$elm_charts$Chart$Attributes$y1(
								config.pinned(p.y)),
								$terezka$elm_charts$Chart$Attributes$x1(
								A2($elm$core$Basics$max, p.x.min, xLimit.min)),
								$terezka$elm_charts$Chart$Attributes$x2(
								A2($elm$core$Basics$min, p.x.max, xLimit.max))
							])),
						config.arrow ? A3(
						$terezka$elm_charts$Chart$Svg$arrow,
						p,
						_List_fromArray(
							[
								$terezka$elm_charts$Chart$Attributes$color(config.color)
							]),
						{
							x: xLimit.max,
							y: config.pinned(p.y)
						}) : $elm$svg$Svg$text('')
					]));
		});
};
var $terezka$elm_charts$Internal$Svg$Floats = {$: 'Floats'};
var $terezka$elm_charts$Chart$LabelsElement = F3(
	function (a, b, c) {
		return {$: 'LabelsElement', a: a, b: b, c: c};
	});
var $terezka$elm_charts$Internal$Svg$defaultLabel = {anchor: $elm$core$Maybe$Nothing, attrs: _List_Nil, border: 'white', borderWidth: 0, color: '#808BAB', fontSize: $elm$core$Maybe$Nothing, rotate: 0, uppercase: false, xOff: 0, yOff: 0};
var $terezka$elm_charts$Internal$Svg$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $terezka$intervals$Intervals$Around = function (a) {
	return {$: 'Around', a: a};
};
var $terezka$intervals$Intervals$around = $terezka$intervals$Intervals$Around;
var $terezka$intervals$Intervals$ceilingTo = F2(
	function (prec, number) {
		return prec * $elm$core$Basics$ceiling(number / prec);
	});
var $terezka$intervals$Intervals$getBeginning = F2(
	function (min, interval) {
		var multiple = min / interval;
		return _Utils_eq(
			multiple,
			$elm$core$Basics$round(multiple)) ? min : A2($terezka$intervals$Intervals$ceilingTo, interval, min);
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $myrho$elm_round$Round$addSign = F2(
	function (signed, str) {
		var isNotZero = A2(
			$elm$core$List$any,
			function (c) {
				return (!_Utils_eq(
					c,
					_Utils_chr('0'))) && (!_Utils_eq(
					c,
					_Utils_chr('.')));
			},
			$elm$core$String$toList(str));
		return _Utils_ap(
			(signed && isNotZero) ? '-' : '',
			str);
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$Char$fromCode = _Char_fromCode;
var $myrho$elm_round$Round$increaseNum = function (_v0) {
	var head = _v0.a;
	var tail = _v0.b;
	if (_Utils_eq(
		head,
		_Utils_chr('9'))) {
		var _v1 = $elm$core$String$uncons(tail);
		if (_v1.$ === 'Nothing') {
			return '01';
		} else {
			var headtail = _v1.a;
			return A2(
				$elm$core$String$cons,
				_Utils_chr('0'),
				$myrho$elm_round$Round$increaseNum(headtail));
		}
	} else {
		var c = $elm$core$Char$toCode(head);
		return ((c >= 48) && (c < 57)) ? A2(
			$elm$core$String$cons,
			$elm$core$Char$fromCode(c + 1),
			tail) : '0';
	}
};
var $elm$core$Basics$isInfinite = _Basics_isInfinite;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $elm$core$String$reverse = _String_reverse;
var $myrho$elm_round$Round$splitComma = function (str) {
	var _v0 = A2($elm$core$String$split, '.', str);
	if (_v0.b) {
		if (_v0.b.b) {
			var before = _v0.a;
			var _v1 = _v0.b;
			var after = _v1.a;
			return _Utils_Tuple2(before, after);
		} else {
			var before = _v0.a;
			return _Utils_Tuple2(before, '0');
		}
	} else {
		return _Utils_Tuple2('0', '0');
	}
};
var $myrho$elm_round$Round$toDecimal = function (fl) {
	var _v0 = A2(
		$elm$core$String$split,
		'e',
		$elm$core$String$fromFloat(
			$elm$core$Basics$abs(fl)));
	if (_v0.b) {
		if (_v0.b.b) {
			var num = _v0.a;
			var _v1 = _v0.b;
			var exp = _v1.a;
			var e = A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(
					A2($elm$core$String$startsWith, '+', exp) ? A2($elm$core$String$dropLeft, 1, exp) : exp));
			var _v2 = $myrho$elm_round$Round$splitComma(num);
			var before = _v2.a;
			var after = _v2.b;
			var total = _Utils_ap(before, after);
			var zeroed = (e < 0) ? A2(
				$elm$core$Maybe$withDefault,
				'0',
				A2(
					$elm$core$Maybe$map,
					function (_v3) {
						var a = _v3.a;
						var b = _v3.b;
						return a + ('.' + b);
					},
					A2(
						$elm$core$Maybe$map,
						$elm$core$Tuple$mapFirst($elm$core$String$fromChar),
						$elm$core$String$uncons(
							_Utils_ap(
								A2(
									$elm$core$String$repeat,
									$elm$core$Basics$abs(e),
									'0'),
								total))))) : A3(
				$elm$core$String$padRight,
				e + 1,
				_Utils_chr('0'),
				total);
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				zeroed);
		} else {
			var num = _v0.a;
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				num);
		}
	} else {
		return '';
	}
};
var $myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if ($elm$core$Basics$isInfinite(fl) || $elm$core$Basics$isNaN(fl)) {
			return $elm$core$String$fromFloat(fl);
		} else {
			var signed = fl < 0;
			var _v0 = $myrho$elm_round$Round$splitComma(
				$myrho$elm_round$Round$toDecimal(
					$elm$core$Basics$abs(fl)));
			var before = _v0.a;
			var after = _v0.b;
			var r = $elm$core$String$length(before) + s;
			var normalized = _Utils_ap(
				A2($elm$core$String$repeat, (-r) + 1, '0'),
				A3(
					$elm$core$String$padRight,
					r,
					_Utils_chr('0'),
					_Utils_ap(before, after)));
			var totalLen = $elm$core$String$length(normalized);
			var roundDigitIndex = A2($elm$core$Basics$max, 1, r);
			var increase = A2(
				functor,
				signed,
				A3($elm$core$String$slice, roundDigitIndex, totalLen, normalized));
			var remains = A3($elm$core$String$slice, 0, roundDigitIndex, normalized);
			var num = increase ? $elm$core$String$reverse(
				A2(
					$elm$core$Maybe$withDefault,
					'1',
					A2(
						$elm$core$Maybe$map,
						$myrho$elm_round$Round$increaseNum,
						$elm$core$String$uncons(
							$elm$core$String$reverse(remains))))) : remains;
			var numLen = $elm$core$String$length(num);
			var numZeroed = (num === '0') ? num : ((s <= 0) ? _Utils_ap(
				num,
				A2(
					$elm$core$String$repeat,
					$elm$core$Basics$abs(s),
					'0')) : ((_Utils_cmp(
				s,
				$elm$core$String$length(after)) < 0) ? (A3($elm$core$String$slice, 0, numLen - s, num) + ('.' + A3($elm$core$String$slice, numLen - s, numLen, num))) : _Utils_ap(
				before + '.',
				A3(
					$elm$core$String$padRight,
					s,
					_Utils_chr('0'),
					after))));
			return A2($myrho$elm_round$Round$addSign, signed, numZeroed);
		}
	});
var $myrho$elm_round$Round$round = $myrho$elm_round$Round$roundFun(
	F2(
		function (signed, str) {
			var _v0 = $elm$core$String$uncons(str);
			if (_v0.$ === 'Nothing') {
				return false;
			} else {
				if ('5' === _v0.a.a.valueOf()) {
					if (_v0.a.b === '') {
						var _v1 = _v0.a;
						return !signed;
					} else {
						var _v2 = _v0.a;
						return true;
					}
				} else {
					var _v3 = _v0.a;
					var _int = _v3.a;
					return function (i) {
						return ((i > 53) && signed) || ((i >= 53) && (!signed));
					}(
						$elm$core$Char$toCode(_int));
				}
			}
		}));
var $terezka$intervals$Intervals$correctFloat = function (prec) {
	return A2(
		$elm$core$Basics$composeR,
		$myrho$elm_round$Round$round(prec),
		A2(
			$elm$core$Basics$composeR,
			$elm$core$String$toFloat,
			$elm$core$Maybe$withDefault(0)));
};
var $terezka$intervals$Intervals$getMultiples = F3(
	function (magnitude, allowDecimals, hasTickAmount) {
		var defaults = hasTickAmount ? _List_fromArray(
			[1, 1.2, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10]) : _List_fromArray(
			[1, 2, 2.5, 5, 10]);
		return allowDecimals ? defaults : ((magnitude === 1) ? A2(
			$elm$core$List$filter,
			function (n) {
				return _Utils_eq(
					$elm$core$Basics$round(n),
					n);
			},
			defaults) : ((magnitude <= 0.1) ? _List_fromArray(
			[1 / magnitude]) : defaults));
	});
var $terezka$intervals$Intervals$getPrecision = function (number) {
	var _v0 = A2(
		$elm$core$String$split,
		'e',
		$elm$core$String$fromFloat(number));
	if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
		var before = _v0.a;
		var _v1 = _v0.b;
		var after = _v1.a;
		return $elm$core$Basics$abs(
			A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(after)));
	} else {
		var _v2 = A2(
			$elm$core$String$split,
			'.',
			$elm$core$String$fromFloat(number));
		if ((_v2.b && _v2.b.b) && (!_v2.b.b.b)) {
			var before = _v2.a;
			var _v3 = _v2.b;
			var after = _v3.a;
			return $elm$core$String$length(after);
		} else {
			return 0;
		}
	}
};
var $terezka$intervals$Intervals$toMagnitude = function (num) {
	return A2(
		$elm$core$Basics$pow,
		10,
		$elm$core$Basics$floor(
			A2($elm$core$Basics$logBase, $elm$core$Basics$e, num) / A2($elm$core$Basics$logBase, $elm$core$Basics$e, 10)));
};
var $terezka$intervals$Intervals$getInterval = F3(
	function (intervalRaw, allowDecimals, hasTickAmount) {
		var magnitude = $terezka$intervals$Intervals$toMagnitude(intervalRaw);
		var multiples = A3($terezka$intervals$Intervals$getMultiples, magnitude, allowDecimals, hasTickAmount);
		var normalized = intervalRaw / magnitude;
		var findMultipleExact = function (multiples_) {
			findMultipleExact:
			while (true) {
				if (multiples_.b) {
					var m1 = multiples_.a;
					var rest = multiples_.b;
					if (_Utils_cmp(m1 * magnitude, intervalRaw) > -1) {
						return m1;
					} else {
						var $temp$multiples_ = rest;
						multiples_ = $temp$multiples_;
						continue findMultipleExact;
					}
				} else {
					return 1;
				}
			}
		};
		var findMultiple = function (multiples_) {
			findMultiple:
			while (true) {
				if (multiples_.b) {
					if (multiples_.b.b) {
						var m1 = multiples_.a;
						var _v2 = multiples_.b;
						var m2 = _v2.a;
						var rest = _v2.b;
						if (_Utils_cmp(normalized, (m1 + m2) / 2) < 1) {
							return m1;
						} else {
							var $temp$multiples_ = A2($elm$core$List$cons, m2, rest);
							multiples_ = $temp$multiples_;
							continue findMultiple;
						}
					} else {
						var m1 = multiples_.a;
						var rest = multiples_.b;
						if (_Utils_cmp(normalized, m1) < 1) {
							return m1;
						} else {
							var $temp$multiples_ = rest;
							multiples_ = $temp$multiples_;
							continue findMultiple;
						}
					}
				} else {
					return 1;
				}
			}
		};
		var multiple = hasTickAmount ? findMultipleExact(multiples) : findMultiple(multiples);
		var precision = $terezka$intervals$Intervals$getPrecision(magnitude) + $terezka$intervals$Intervals$getPrecision(multiple);
		return A2($terezka$intervals$Intervals$correctFloat, precision, multiple * magnitude);
	});
var $terezka$intervals$Intervals$positions = F5(
	function (range, beginning, interval, m, acc) {
		positions:
		while (true) {
			var nextPosition = A2(
				$terezka$intervals$Intervals$correctFloat,
				$terezka$intervals$Intervals$getPrecision(interval),
				beginning + (m * interval));
			if (_Utils_cmp(nextPosition, range.max) > 0) {
				return acc;
			} else {
				var $temp$range = range,
					$temp$beginning = beginning,
					$temp$interval = interval,
					$temp$m = m + 1,
					$temp$acc = _Utils_ap(
					acc,
					_List_fromArray(
						[nextPosition]));
				range = $temp$range;
				beginning = $temp$beginning;
				interval = $temp$interval;
				m = $temp$m;
				acc = $temp$acc;
				continue positions;
			}
		}
	});
var $terezka$intervals$Intervals$values = F4(
	function (allowDecimals, exact, amountRough, range) {
		var intervalRough = (range.max - range.min) / amountRough;
		var interval = A3($terezka$intervals$Intervals$getInterval, intervalRough, allowDecimals, exact);
		var intervalSafe = (!interval) ? 1 : interval;
		var beginning = A2($terezka$intervals$Intervals$getBeginning, range.min, intervalSafe);
		var amountRoughSafe = (!amountRough) ? 1 : amountRough;
		return A5($terezka$intervals$Intervals$positions, range, beginning, intervalSafe, 0, _List_Nil);
	});
var $terezka$intervals$Intervals$floats = function (amount) {
	if (amount.$ === 'Exactly') {
		var number = amount.a;
		return A3($terezka$intervals$Intervals$values, true, true, number);
	} else {
		var number = amount.a;
		return A3($terezka$intervals$Intervals$values, true, false, number);
	}
};
var $terezka$elm_charts$Internal$Svg$floats = $terezka$elm_charts$Internal$Svg$Generator(
	F2(
		function (i, b) {
			return A2(
				$terezka$intervals$Intervals$floats,
				$terezka$intervals$Intervals$around(i),
				{max: b.max, min: b.min});
		}));
var $terezka$elm_charts$Chart$Svg$floats = $terezka$elm_charts$Internal$Svg$floats;
var $ryannhg$date_format$DateFormat$Language$Language = F6(
	function (toMonthName, toMonthAbbreviation, toWeekdayName, toWeekdayAbbreviation, toAmPm, toOrdinalSuffix) {
		return {toAmPm: toAmPm, toMonthAbbreviation: toMonthAbbreviation, toMonthName: toMonthName, toOrdinalSuffix: toOrdinalSuffix, toWeekdayAbbreviation: toWeekdayAbbreviation, toWeekdayName: toWeekdayName};
	});
var $ryannhg$date_format$DateFormat$Language$toEnglishAmPm = function (hour) {
	return (hour > 11) ? 'pm' : 'am';
};
var $ryannhg$date_format$DateFormat$Language$toEnglishMonthName = function (month) {
	switch (month.$) {
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
var $elm$core$Basics$modBy = _Basics_modBy;
var $ryannhg$date_format$DateFormat$Language$toEnglishSuffix = function (num) {
	var _v0 = A2($elm$core$Basics$modBy, 100, num);
	switch (_v0) {
		case 11:
			return 'th';
		case 12:
			return 'th';
		case 13:
			return 'th';
		default:
			var _v1 = A2($elm$core$Basics$modBy, 10, num);
			switch (_v1) {
				case 1:
					return 'st';
				case 2:
					return 'nd';
				case 3:
					return 'rd';
				default:
					return 'th';
			}
	}
};
var $ryannhg$date_format$DateFormat$Language$toEnglishWeekdayName = function (weekday) {
	switch (weekday.$) {
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
var $ryannhg$date_format$DateFormat$Language$english = A6(
	$ryannhg$date_format$DateFormat$Language$Language,
	$ryannhg$date_format$DateFormat$Language$toEnglishMonthName,
	A2(
		$elm$core$Basics$composeR,
		$ryannhg$date_format$DateFormat$Language$toEnglishMonthName,
		$elm$core$String$left(3)),
	$ryannhg$date_format$DateFormat$Language$toEnglishWeekdayName,
	A2(
		$elm$core$Basics$composeR,
		$ryannhg$date_format$DateFormat$Language$toEnglishWeekdayName,
		$elm$core$String$left(3)),
	$ryannhg$date_format$DateFormat$Language$toEnglishAmPm,
	$ryannhg$date_format$DateFormat$Language$toEnglishSuffix);
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.start, posixMinutes) < 0) {
					return posixMinutes + era.offset;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var $elm$time$Time$toAdjustedMinutes = F2(
	function (_v0, time) {
		var defaultOffset = _v0.a;
		var eras = _v0.b;
		return A3(
			$elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var $elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			24,
			A2(
				$elm$time$Time$flooredDiv,
				A2($elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var $ryannhg$date_format$DateFormat$amPm = F3(
	function (language, zone, posix) {
		return language.toAmPm(
			A2($elm$time$Time$toHour, zone, posix));
	});
var $elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2($elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		day: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		month: month,
		year: year + ((month <= 2) ? 1 : 0)
	};
};
var $elm$time$Time$toDay = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).day;
	});
var $ryannhg$date_format$DateFormat$dayOfMonth = $elm$time$Time$toDay;
var $elm$time$Time$Sun = {$: 'Sun'};
var $elm$time$Time$Fri = {$: 'Fri'};
var $elm$time$Time$Mon = {$: 'Mon'};
var $elm$time$Time$Sat = {$: 'Sat'};
var $elm$time$Time$Thu = {$: 'Thu'};
var $elm$time$Time$Tue = {$: 'Tue'};
var $elm$time$Time$Wed = {$: 'Wed'};
var $ryannhg$date_format$DateFormat$days = _List_fromArray(
	[$elm$time$Time$Sun, $elm$time$Time$Mon, $elm$time$Time$Tue, $elm$time$Time$Wed, $elm$time$Time$Thu, $elm$time$Time$Fri, $elm$time$Time$Sat]);
var $elm$time$Time$toWeekday = F2(
	function (zone, time) {
		var _v0 = A2(
			$elm$core$Basics$modBy,
			7,
			A2(
				$elm$time$Time$flooredDiv,
				A2($elm$time$Time$toAdjustedMinutes, zone, time),
				60 * 24));
		switch (_v0) {
			case 0:
				return $elm$time$Time$Thu;
			case 1:
				return $elm$time$Time$Fri;
			case 2:
				return $elm$time$Time$Sat;
			case 3:
				return $elm$time$Time$Sun;
			case 4:
				return $elm$time$Time$Mon;
			case 5:
				return $elm$time$Time$Tue;
			default:
				return $elm$time$Time$Wed;
		}
	});
var $ryannhg$date_format$DateFormat$dayOfWeek = F2(
	function (zone, posix) {
		return function (_v1) {
			var i = _v1.a;
			return i;
		}(
			A2(
				$elm$core$Maybe$withDefault,
				_Utils_Tuple2(0, $elm$time$Time$Sun),
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						function (_v0) {
							var day = _v0.b;
							return _Utils_eq(
								day,
								A2($elm$time$Time$toWeekday, zone, posix));
						},
						A2(
							$elm$core$List$indexedMap,
							F2(
								function (i, day) {
									return _Utils_Tuple2(i, day);
								}),
							$ryannhg$date_format$DateFormat$days)))));
	});
var $ryannhg$date_format$DateFormat$isLeapYear = function (year_) {
	return (!(!A2($elm$core$Basics$modBy, 4, year_))) ? false : ((!(!A2($elm$core$Basics$modBy, 100, year_))) ? true : ((!(!A2($elm$core$Basics$modBy, 400, year_))) ? false : true));
};
var $ryannhg$date_format$DateFormat$daysInMonth = F2(
	function (year_, month) {
		switch (month.$) {
			case 'Jan':
				return 31;
			case 'Feb':
				return $ryannhg$date_format$DateFormat$isLeapYear(year_) ? 29 : 28;
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
var $elm$time$Time$Jan = {$: 'Jan'};
var $elm$time$Time$Apr = {$: 'Apr'};
var $elm$time$Time$Aug = {$: 'Aug'};
var $elm$time$Time$Dec = {$: 'Dec'};
var $elm$time$Time$Feb = {$: 'Feb'};
var $elm$time$Time$Jul = {$: 'Jul'};
var $elm$time$Time$Jun = {$: 'Jun'};
var $elm$time$Time$Mar = {$: 'Mar'};
var $elm$time$Time$May = {$: 'May'};
var $elm$time$Time$Nov = {$: 'Nov'};
var $elm$time$Time$Oct = {$: 'Oct'};
var $elm$time$Time$Sep = {$: 'Sep'};
var $ryannhg$date_format$DateFormat$months = _List_fromArray(
	[$elm$time$Time$Jan, $elm$time$Time$Feb, $elm$time$Time$Mar, $elm$time$Time$Apr, $elm$time$Time$May, $elm$time$Time$Jun, $elm$time$Time$Jul, $elm$time$Time$Aug, $elm$time$Time$Sep, $elm$time$Time$Oct, $elm$time$Time$Nov, $elm$time$Time$Dec]);
var $elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _v0 = $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).month;
		switch (_v0) {
			case 1:
				return $elm$time$Time$Jan;
			case 2:
				return $elm$time$Time$Feb;
			case 3:
				return $elm$time$Time$Mar;
			case 4:
				return $elm$time$Time$Apr;
			case 5:
				return $elm$time$Time$May;
			case 6:
				return $elm$time$Time$Jun;
			case 7:
				return $elm$time$Time$Jul;
			case 8:
				return $elm$time$Time$Aug;
			case 9:
				return $elm$time$Time$Sep;
			case 10:
				return $elm$time$Time$Oct;
			case 11:
				return $elm$time$Time$Nov;
			default:
				return $elm$time$Time$Dec;
		}
	});
var $ryannhg$date_format$DateFormat$monthPair = F2(
	function (zone, posix) {
		return A2(
			$elm$core$Maybe$withDefault,
			_Utils_Tuple2(0, $elm$time$Time$Jan),
			$elm$core$List$head(
				A2(
					$elm$core$List$filter,
					function (_v0) {
						var i = _v0.a;
						var m = _v0.b;
						return _Utils_eq(
							m,
							A2($elm$time$Time$toMonth, zone, posix));
					},
					A2(
						$elm$core$List$indexedMap,
						F2(
							function (a, b) {
								return _Utils_Tuple2(a, b);
							}),
						$ryannhg$date_format$DateFormat$months))));
	});
var $ryannhg$date_format$DateFormat$monthNumber_ = F2(
	function (zone, posix) {
		return 1 + function (_v0) {
			var i = _v0.a;
			var m = _v0.b;
			return i;
		}(
			A2($ryannhg$date_format$DateFormat$monthPair, zone, posix));
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $elm$time$Time$toYear = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).year;
	});
var $ryannhg$date_format$DateFormat$dayOfYear = F2(
	function (zone, posix) {
		var monthsBeforeThisOne = A2(
			$elm$core$List$take,
			A2($ryannhg$date_format$DateFormat$monthNumber_, zone, posix) - 1,
			$ryannhg$date_format$DateFormat$months);
		var daysBeforeThisMonth = $elm$core$List$sum(
			A2(
				$elm$core$List$map,
				$ryannhg$date_format$DateFormat$daysInMonth(
					A2($elm$time$Time$toYear, zone, posix)),
				monthsBeforeThisOne));
		return daysBeforeThisMonth + A2($ryannhg$date_format$DateFormat$dayOfMonth, zone, posix);
	});
var $ryannhg$date_format$DateFormat$quarter = F2(
	function (zone, posix) {
		return (A2($ryannhg$date_format$DateFormat$monthNumber_, zone, posix) / 4) | 0;
	});
var $elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			$elm$core$String$slice,
			-n,
			$elm$core$String$length(string),
			string);
	});
var $ryannhg$date_format$DateFormat$toFixedLength = F2(
	function (totalChars, num) {
		var numStr = $elm$core$String$fromInt(num);
		var numZerosNeeded = totalChars - $elm$core$String$length(numStr);
		var zeros = A2(
			$elm$core$String$join,
			'',
			A2(
				$elm$core$List$map,
				function (_v0) {
					return '0';
				},
				A2($elm$core$List$range, 1, numZerosNeeded)));
		return _Utils_ap(zeros, numStr);
	});
var $elm$core$String$toLower = _String_toLower;
var $elm$time$Time$toMillis = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			1000,
			$elm$time$Time$posixToMillis(time));
	});
var $elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2($elm$time$Time$toAdjustedMinutes, zone, time));
	});
var $ryannhg$date_format$DateFormat$toNonMilitary = function (num) {
	return (!num) ? 12 : ((num <= 12) ? num : (num - 12));
};
var $elm$time$Time$toSecond = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				1000));
	});
var $elm$core$String$toUpper = _String_toUpper;
var $ryannhg$date_format$DateFormat$millisecondsPerYear = $elm$core$Basics$round((((1000 * 60) * 60) * 24) * 365.25);
var $ryannhg$date_format$DateFormat$firstDayOfYear = F2(
	function (zone, time) {
		return $elm$time$Time$millisToPosix(
			$ryannhg$date_format$DateFormat$millisecondsPerYear * A2($elm$time$Time$toYear, zone, time));
	});
var $ryannhg$date_format$DateFormat$weekOfYear = F2(
	function (zone, posix) {
		var firstDay = A2($ryannhg$date_format$DateFormat$firstDayOfYear, zone, posix);
		var firstDayOffset = A2($ryannhg$date_format$DateFormat$dayOfWeek, zone, firstDay);
		var daysSoFar = A2($ryannhg$date_format$DateFormat$dayOfYear, zone, posix);
		return (((daysSoFar + firstDayOffset) / 7) | 0) + 1;
	});
var $ryannhg$date_format$DateFormat$year = F2(
	function (zone, time) {
		return $elm$core$String$fromInt(
			A2($elm$time$Time$toYear, zone, time));
	});
var $ryannhg$date_format$DateFormat$piece = F4(
	function (language, zone, posix, token) {
		switch (token.$) {
			case 'MonthNumber':
				return $elm$core$String$fromInt(
					A2($ryannhg$date_format$DateFormat$monthNumber_, zone, posix));
			case 'MonthSuffix':
				return function (num) {
					return _Utils_ap(
						$elm$core$String$fromInt(num),
						language.toOrdinalSuffix(num));
				}(
					A2($ryannhg$date_format$DateFormat$monthNumber_, zone, posix));
			case 'MonthFixed':
				return A2(
					$ryannhg$date_format$DateFormat$toFixedLength,
					2,
					A2($ryannhg$date_format$DateFormat$monthNumber_, zone, posix));
			case 'MonthNameAbbreviated':
				return language.toMonthAbbreviation(
					A2($elm$time$Time$toMonth, zone, posix));
			case 'MonthNameFull':
				return language.toMonthName(
					A2($elm$time$Time$toMonth, zone, posix));
			case 'QuarterNumber':
				return $elm$core$String$fromInt(
					1 + A2($ryannhg$date_format$DateFormat$quarter, zone, posix));
			case 'QuarterSuffix':
				return function (num) {
					return _Utils_ap(
						$elm$core$String$fromInt(num),
						language.toOrdinalSuffix(num));
				}(
					1 + A2($ryannhg$date_format$DateFormat$quarter, zone, posix));
			case 'DayOfMonthNumber':
				return $elm$core$String$fromInt(
					A2($ryannhg$date_format$DateFormat$dayOfMonth, zone, posix));
			case 'DayOfMonthSuffix':
				return function (num) {
					return _Utils_ap(
						$elm$core$String$fromInt(num),
						language.toOrdinalSuffix(num));
				}(
					A2($ryannhg$date_format$DateFormat$dayOfMonth, zone, posix));
			case 'DayOfMonthFixed':
				return A2(
					$ryannhg$date_format$DateFormat$toFixedLength,
					2,
					A2($ryannhg$date_format$DateFormat$dayOfMonth, zone, posix));
			case 'DayOfYearNumber':
				return $elm$core$String$fromInt(
					A2($ryannhg$date_format$DateFormat$dayOfYear, zone, posix));
			case 'DayOfYearSuffix':
				return function (num) {
					return _Utils_ap(
						$elm$core$String$fromInt(num),
						language.toOrdinalSuffix(num));
				}(
					A2($ryannhg$date_format$DateFormat$dayOfYear, zone, posix));
			case 'DayOfYearFixed':
				return A2(
					$ryannhg$date_format$DateFormat$toFixedLength,
					3,
					A2($ryannhg$date_format$DateFormat$dayOfYear, zone, posix));
			case 'DayOfWeekNumber':
				return $elm$core$String$fromInt(
					A2($ryannhg$date_format$DateFormat$dayOfWeek, zone, posix));
			case 'DayOfWeekSuffix':
				return function (num) {
					return _Utils_ap(
						$elm$core$String$fromInt(num),
						language.toOrdinalSuffix(num));
				}(
					A2($ryannhg$date_format$DateFormat$dayOfWeek, zone, posix));
			case 'DayOfWeekNameAbbreviated':
				return language.toWeekdayAbbreviation(
					A2($elm$time$Time$toWeekday, zone, posix));
			case 'DayOfWeekNameFull':
				return language.toWeekdayName(
					A2($elm$time$Time$toWeekday, zone, posix));
			case 'WeekOfYearNumber':
				return $elm$core$String$fromInt(
					A2($ryannhg$date_format$DateFormat$weekOfYear, zone, posix));
			case 'WeekOfYearSuffix':
				return function (num) {
					return _Utils_ap(
						$elm$core$String$fromInt(num),
						language.toOrdinalSuffix(num));
				}(
					A2($ryannhg$date_format$DateFormat$weekOfYear, zone, posix));
			case 'WeekOfYearFixed':
				return A2(
					$ryannhg$date_format$DateFormat$toFixedLength,
					2,
					A2($ryannhg$date_format$DateFormat$weekOfYear, zone, posix));
			case 'YearNumberLastTwo':
				return A2(
					$elm$core$String$right,
					2,
					A2($ryannhg$date_format$DateFormat$year, zone, posix));
			case 'YearNumber':
				return A2($ryannhg$date_format$DateFormat$year, zone, posix);
			case 'AmPmUppercase':
				return $elm$core$String$toUpper(
					A3($ryannhg$date_format$DateFormat$amPm, language, zone, posix));
			case 'AmPmLowercase':
				return $elm$core$String$toLower(
					A3($ryannhg$date_format$DateFormat$amPm, language, zone, posix));
			case 'HourMilitaryNumber':
				return $elm$core$String$fromInt(
					A2($elm$time$Time$toHour, zone, posix));
			case 'HourMilitaryFixed':
				return A2(
					$ryannhg$date_format$DateFormat$toFixedLength,
					2,
					A2($elm$time$Time$toHour, zone, posix));
			case 'HourNumber':
				return $elm$core$String$fromInt(
					$ryannhg$date_format$DateFormat$toNonMilitary(
						A2($elm$time$Time$toHour, zone, posix)));
			case 'HourFixed':
				return A2(
					$ryannhg$date_format$DateFormat$toFixedLength,
					2,
					$ryannhg$date_format$DateFormat$toNonMilitary(
						A2($elm$time$Time$toHour, zone, posix)));
			case 'HourMilitaryFromOneNumber':
				return $elm$core$String$fromInt(
					1 + A2($elm$time$Time$toHour, zone, posix));
			case 'HourMilitaryFromOneFixed':
				return A2(
					$ryannhg$date_format$DateFormat$toFixedLength,
					2,
					1 + A2($elm$time$Time$toHour, zone, posix));
			case 'MinuteNumber':
				return $elm$core$String$fromInt(
					A2($elm$time$Time$toMinute, zone, posix));
			case 'MinuteFixed':
				return A2(
					$ryannhg$date_format$DateFormat$toFixedLength,
					2,
					A2($elm$time$Time$toMinute, zone, posix));
			case 'SecondNumber':
				return $elm$core$String$fromInt(
					A2($elm$time$Time$toSecond, zone, posix));
			case 'SecondFixed':
				return A2(
					$ryannhg$date_format$DateFormat$toFixedLength,
					2,
					A2($elm$time$Time$toSecond, zone, posix));
			case 'MillisecondNumber':
				return $elm$core$String$fromInt(
					A2($elm$time$Time$toMillis, zone, posix));
			case 'MillisecondFixed':
				return A2(
					$ryannhg$date_format$DateFormat$toFixedLength,
					3,
					A2($elm$time$Time$toMillis, zone, posix));
			default:
				var string = token.a;
				return string;
		}
	});
var $ryannhg$date_format$DateFormat$formatWithLanguage = F4(
	function (language, tokens, zone, time) {
		return A2(
			$elm$core$String$join,
			'',
			A2(
				$elm$core$List$map,
				A3($ryannhg$date_format$DateFormat$piece, language, zone, time),
				tokens));
	});
var $ryannhg$date_format$DateFormat$format = $ryannhg$date_format$DateFormat$formatWithLanguage($ryannhg$date_format$DateFormat$Language$english);
var $ryannhg$date_format$DateFormat$HourMilitaryFixed = {$: 'HourMilitaryFixed'};
var $ryannhg$date_format$DateFormat$hourMilitaryFixed = $ryannhg$date_format$DateFormat$HourMilitaryFixed;
var $ryannhg$date_format$DateFormat$MinuteFixed = {$: 'MinuteFixed'};
var $ryannhg$date_format$DateFormat$minuteFixed = $ryannhg$date_format$DateFormat$MinuteFixed;
var $ryannhg$date_format$DateFormat$Text = function (a) {
	return {$: 'Text', a: a};
};
var $ryannhg$date_format$DateFormat$text = $ryannhg$date_format$DateFormat$Text;
var $terezka$elm_charts$Internal$Svg$formatClock = $ryannhg$date_format$DateFormat$format(
	_List_fromArray(
		[
			$ryannhg$date_format$DateFormat$hourMilitaryFixed,
			$ryannhg$date_format$DateFormat$text(':'),
			$ryannhg$date_format$DateFormat$minuteFixed
		]));
var $ryannhg$date_format$DateFormat$MillisecondFixed = {$: 'MillisecondFixed'};
var $ryannhg$date_format$DateFormat$millisecondFixed = $ryannhg$date_format$DateFormat$MillisecondFixed;
var $ryannhg$date_format$DateFormat$SecondFixed = {$: 'SecondFixed'};
var $ryannhg$date_format$DateFormat$secondFixed = $ryannhg$date_format$DateFormat$SecondFixed;
var $terezka$elm_charts$Internal$Svg$formatClockMillis = $ryannhg$date_format$DateFormat$format(
	_List_fromArray(
		[
			$ryannhg$date_format$DateFormat$hourMilitaryFixed,
			$ryannhg$date_format$DateFormat$text(':'),
			$ryannhg$date_format$DateFormat$minuteFixed,
			$ryannhg$date_format$DateFormat$text(':'),
			$ryannhg$date_format$DateFormat$secondFixed,
			$ryannhg$date_format$DateFormat$text(':'),
			$ryannhg$date_format$DateFormat$millisecondFixed
		]));
var $terezka$elm_charts$Internal$Svg$formatClockSecond = $ryannhg$date_format$DateFormat$format(
	_List_fromArray(
		[
			$ryannhg$date_format$DateFormat$hourMilitaryFixed,
			$ryannhg$date_format$DateFormat$text(':'),
			$ryannhg$date_format$DateFormat$minuteFixed,
			$ryannhg$date_format$DateFormat$text(':'),
			$ryannhg$date_format$DateFormat$secondFixed
		]));
var $ryannhg$date_format$DateFormat$DayOfMonthNumber = {$: 'DayOfMonthNumber'};
var $ryannhg$date_format$DateFormat$dayOfMonthNumber = $ryannhg$date_format$DateFormat$DayOfMonthNumber;
var $ryannhg$date_format$DateFormat$MonthNumber = {$: 'MonthNumber'};
var $ryannhg$date_format$DateFormat$monthNumber = $ryannhg$date_format$DateFormat$MonthNumber;
var $terezka$elm_charts$Internal$Svg$formatDate = $ryannhg$date_format$DateFormat$format(
	_List_fromArray(
		[
			$ryannhg$date_format$DateFormat$monthNumber,
			$ryannhg$date_format$DateFormat$text('/'),
			$ryannhg$date_format$DateFormat$dayOfMonthNumber
		]));
var $ryannhg$date_format$DateFormat$MonthNameAbbreviated = {$: 'MonthNameAbbreviated'};
var $ryannhg$date_format$DateFormat$monthNameAbbreviated = $ryannhg$date_format$DateFormat$MonthNameAbbreviated;
var $terezka$elm_charts$Internal$Svg$formatMonth = $ryannhg$date_format$DateFormat$format(
	_List_fromArray(
		[$ryannhg$date_format$DateFormat$monthNameAbbreviated]));
var $ryannhg$date_format$DateFormat$DayOfWeekNameFull = {$: 'DayOfWeekNameFull'};
var $ryannhg$date_format$DateFormat$dayOfWeekNameFull = $ryannhg$date_format$DateFormat$DayOfWeekNameFull;
var $terezka$elm_charts$Internal$Svg$formatWeekday = $ryannhg$date_format$DateFormat$format(
	_List_fromArray(
		[$ryannhg$date_format$DateFormat$dayOfWeekNameFull]));
var $ryannhg$date_format$DateFormat$YearNumber = {$: 'YearNumber'};
var $ryannhg$date_format$DateFormat$yearNumber = $ryannhg$date_format$DateFormat$YearNumber;
var $terezka$elm_charts$Internal$Svg$formatYear = $ryannhg$date_format$DateFormat$format(
	_List_fromArray(
		[$ryannhg$date_format$DateFormat$yearNumber]));
var $terezka$elm_charts$Internal$Svg$formatTime = F2(
	function (zone, time) {
		var _v0 = A2($elm$core$Maybe$withDefault, time.unit, time.change);
		switch (_v0.$) {
			case 'Millisecond':
				return A2($terezka$elm_charts$Internal$Svg$formatClockMillis, zone, time.timestamp);
			case 'Second':
				return A2($terezka$elm_charts$Internal$Svg$formatClockSecond, zone, time.timestamp);
			case 'Minute':
				return A2($terezka$elm_charts$Internal$Svg$formatClock, zone, time.timestamp);
			case 'Hour':
				return A2($terezka$elm_charts$Internal$Svg$formatClock, zone, time.timestamp);
			case 'Day':
				return (time.multiple === 7) ? A2($terezka$elm_charts$Internal$Svg$formatWeekday, zone, time.timestamp) : A2($terezka$elm_charts$Internal$Svg$formatDate, zone, time.timestamp);
			case 'Month':
				return A2($terezka$elm_charts$Internal$Svg$formatMonth, zone, time.timestamp);
			default:
				return A2($terezka$elm_charts$Internal$Svg$formatYear, zone, time.timestamp);
		}
	});
var $terezka$elm_charts$Chart$Svg$formatTime = $terezka$elm_charts$Internal$Svg$formatTime;
var $terezka$elm_charts$Internal$Svg$generate = F3(
	function (amount, _v0, limits) {
		var func = _v0.a;
		return A2(func, amount, limits);
	});
var $terezka$elm_charts$Chart$Svg$generate = $terezka$elm_charts$Internal$Svg$generate;
var $terezka$intervals$Intervals$ints = F2(
	function (amount, range) {
		return A2(
			$elm$core$List$map,
			$elm$core$Basics$round,
			function () {
				if (amount.$ === 'Exactly') {
					var number = amount.a;
					return A4($terezka$intervals$Intervals$values, false, true, number, range);
				} else {
					var number = amount.a;
					return A4($terezka$intervals$Intervals$values, false, false, number, range);
				}
			}());
	});
var $terezka$elm_charts$Internal$Svg$ints = $terezka$elm_charts$Internal$Svg$Generator(
	F2(
		function (i, b) {
			return A2(
				$terezka$intervals$Intervals$ints,
				$terezka$intervals$Intervals$around(i),
				{max: b.max, min: b.min});
		}));
var $terezka$elm_charts$Chart$Svg$ints = $terezka$elm_charts$Internal$Svg$ints;
var $terezka$intervals$Intervals$Day = {$: 'Day'};
var $terezka$intervals$Intervals$Hour = {$: 'Hour'};
var $terezka$intervals$Intervals$Millisecond = {$: 'Millisecond'};
var $terezka$intervals$Intervals$Minute = {$: 'Minute'};
var $terezka$intervals$Intervals$Month = {$: 'Month'};
var $terezka$intervals$Intervals$Second = {$: 'Second'};
var $terezka$intervals$Intervals$Year = {$: 'Year'};
var $justinmimbs$time_extra$Time$Extra$Day = {$: 'Day'};
var $justinmimbs$date$Date$Days = {$: 'Days'};
var $justinmimbs$time_extra$Time$Extra$Millisecond = {$: 'Millisecond'};
var $justinmimbs$time_extra$Time$Extra$Month = {$: 'Month'};
var $justinmimbs$date$Date$Months = {$: 'Months'};
var $justinmimbs$date$Date$RD = function (a) {
	return {$: 'RD', a: a};
};
var $justinmimbs$date$Date$isLeapYear = function (y) {
	return ((!A2($elm$core$Basics$modBy, 4, y)) && (!(!A2($elm$core$Basics$modBy, 100, y)))) || (!A2($elm$core$Basics$modBy, 400, y));
};
var $justinmimbs$date$Date$daysBeforeMonth = F2(
	function (y, m) {
		var leapDays = $justinmimbs$date$Date$isLeapYear(y) ? 1 : 0;
		switch (m.$) {
			case 'Jan':
				return 0;
			case 'Feb':
				return 31;
			case 'Mar':
				return 59 + leapDays;
			case 'Apr':
				return 90 + leapDays;
			case 'May':
				return 120 + leapDays;
			case 'Jun':
				return 151 + leapDays;
			case 'Jul':
				return 181 + leapDays;
			case 'Aug':
				return 212 + leapDays;
			case 'Sep':
				return 243 + leapDays;
			case 'Oct':
				return 273 + leapDays;
			case 'Nov':
				return 304 + leapDays;
			default:
				return 334 + leapDays;
		}
	});
var $justinmimbs$date$Date$floorDiv = F2(
	function (a, b) {
		return $elm$core$Basics$floor(a / b);
	});
var $justinmimbs$date$Date$daysBeforeYear = function (y1) {
	var y = y1 - 1;
	var leapYears = (A2($justinmimbs$date$Date$floorDiv, y, 4) - A2($justinmimbs$date$Date$floorDiv, y, 100)) + A2($justinmimbs$date$Date$floorDiv, y, 400);
	return (365 * y) + leapYears;
};
var $justinmimbs$date$Date$daysInMonth = F2(
	function (y, m) {
		switch (m.$) {
			case 'Jan':
				return 31;
			case 'Feb':
				return $justinmimbs$date$Date$isLeapYear(y) ? 29 : 28;
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
var $justinmimbs$date$Date$monthToNumber = function (m) {
	switch (m.$) {
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
var $justinmimbs$date$Date$numberToMonth = function (mn) {
	var _v0 = A2($elm$core$Basics$max, 1, mn);
	switch (_v0) {
		case 1:
			return $elm$time$Time$Jan;
		case 2:
			return $elm$time$Time$Feb;
		case 3:
			return $elm$time$Time$Mar;
		case 4:
			return $elm$time$Time$Apr;
		case 5:
			return $elm$time$Time$May;
		case 6:
			return $elm$time$Time$Jun;
		case 7:
			return $elm$time$Time$Jul;
		case 8:
			return $elm$time$Time$Aug;
		case 9:
			return $elm$time$Time$Sep;
		case 10:
			return $elm$time$Time$Oct;
		case 11:
			return $elm$time$Time$Nov;
		default:
			return $elm$time$Time$Dec;
	}
};
var $justinmimbs$date$Date$toCalendarDateHelp = F3(
	function (y, m, d) {
		toCalendarDateHelp:
		while (true) {
			var monthDays = A2($justinmimbs$date$Date$daysInMonth, y, m);
			var mn = $justinmimbs$date$Date$monthToNumber(m);
			if ((mn < 12) && (_Utils_cmp(d, monthDays) > 0)) {
				var $temp$y = y,
					$temp$m = $justinmimbs$date$Date$numberToMonth(mn + 1),
					$temp$d = d - monthDays;
				y = $temp$y;
				m = $temp$m;
				d = $temp$d;
				continue toCalendarDateHelp;
			} else {
				return {day: d, month: m, year: y};
			}
		}
	});
var $justinmimbs$date$Date$divWithRemainder = F2(
	function (a, b) {
		return _Utils_Tuple2(
			A2($justinmimbs$date$Date$floorDiv, a, b),
			A2($elm$core$Basics$modBy, b, a));
	});
var $justinmimbs$date$Date$year = function (_v0) {
	var rd = _v0.a;
	var _v1 = A2($justinmimbs$date$Date$divWithRemainder, rd, 146097);
	var n400 = _v1.a;
	var r400 = _v1.b;
	var _v2 = A2($justinmimbs$date$Date$divWithRemainder, r400, 36524);
	var n100 = _v2.a;
	var r100 = _v2.b;
	var _v3 = A2($justinmimbs$date$Date$divWithRemainder, r100, 1461);
	var n4 = _v3.a;
	var r4 = _v3.b;
	var _v4 = A2($justinmimbs$date$Date$divWithRemainder, r4, 365);
	var n1 = _v4.a;
	var r1 = _v4.b;
	var n = (!r1) ? 0 : 1;
	return ((((n400 * 400) + (n100 * 100)) + (n4 * 4)) + n1) + n;
};
var $justinmimbs$date$Date$toOrdinalDate = function (_v0) {
	var rd = _v0.a;
	var y = $justinmimbs$date$Date$year(
		$justinmimbs$date$Date$RD(rd));
	return {
		ordinalDay: rd - $justinmimbs$date$Date$daysBeforeYear(y),
		year: y
	};
};
var $justinmimbs$date$Date$toCalendarDate = function (_v0) {
	var rd = _v0.a;
	var date = $justinmimbs$date$Date$toOrdinalDate(
		$justinmimbs$date$Date$RD(rd));
	return A3($justinmimbs$date$Date$toCalendarDateHelp, date.year, $elm$time$Time$Jan, date.ordinalDay);
};
var $justinmimbs$date$Date$add = F3(
	function (unit, n, _v0) {
		var rd = _v0.a;
		switch (unit.$) {
			case 'Years':
				return A3(
					$justinmimbs$date$Date$add,
					$justinmimbs$date$Date$Months,
					12 * n,
					$justinmimbs$date$Date$RD(rd));
			case 'Months':
				var date = $justinmimbs$date$Date$toCalendarDate(
					$justinmimbs$date$Date$RD(rd));
				var wholeMonths = ((12 * (date.year - 1)) + ($justinmimbs$date$Date$monthToNumber(date.month) - 1)) + n;
				var m = $justinmimbs$date$Date$numberToMonth(
					A2($elm$core$Basics$modBy, 12, wholeMonths) + 1);
				var y = A2($justinmimbs$date$Date$floorDiv, wholeMonths, 12) + 1;
				return $justinmimbs$date$Date$RD(
					($justinmimbs$date$Date$daysBeforeYear(y) + A2($justinmimbs$date$Date$daysBeforeMonth, y, m)) + A2(
						$elm$core$Basics$min,
						date.day,
						A2($justinmimbs$date$Date$daysInMonth, y, m)));
			case 'Weeks':
				return $justinmimbs$date$Date$RD(rd + (7 * n));
			default:
				return $justinmimbs$date$Date$RD(rd + n);
		}
	});
var $justinmimbs$date$Date$fromCalendarDate = F3(
	function (y, m, d) {
		return $justinmimbs$date$Date$RD(
			($justinmimbs$date$Date$daysBeforeYear(y) + A2($justinmimbs$date$Date$daysBeforeMonth, y, m)) + A3(
				$elm$core$Basics$clamp,
				1,
				A2($justinmimbs$date$Date$daysInMonth, y, m),
				d));
	});
var $justinmimbs$date$Date$fromPosix = F2(
	function (zone, posix) {
		return A3(
			$justinmimbs$date$Date$fromCalendarDate,
			A2($elm$time$Time$toYear, zone, posix),
			A2($elm$time$Time$toMonth, zone, posix),
			A2($elm$time$Time$toDay, zone, posix));
	});
var $justinmimbs$date$Date$toRataDie = function (_v0) {
	var rd = _v0.a;
	return rd;
};
var $justinmimbs$time_extra$Time$Extra$dateToMillis = function (date) {
	var daysSinceEpoch = $justinmimbs$date$Date$toRataDie(date) - 719163;
	return daysSinceEpoch * 86400000;
};
var $justinmimbs$time_extra$Time$Extra$timeFromClock = F4(
	function (hour, minute, second, millisecond) {
		return (((hour * 3600000) + (minute * 60000)) + (second * 1000)) + millisecond;
	});
var $justinmimbs$time_extra$Time$Extra$timeFromPosix = F2(
	function (zone, posix) {
		return A4(
			$justinmimbs$time_extra$Time$Extra$timeFromClock,
			A2($elm$time$Time$toHour, zone, posix),
			A2($elm$time$Time$toMinute, zone, posix),
			A2($elm$time$Time$toSecond, zone, posix),
			A2($elm$time$Time$toMillis, zone, posix));
	});
var $justinmimbs$time_extra$Time$Extra$toOffset = F2(
	function (zone, posix) {
		var millis = $elm$time$Time$posixToMillis(posix);
		var localMillis = $justinmimbs$time_extra$Time$Extra$dateToMillis(
			A2($justinmimbs$date$Date$fromPosix, zone, posix)) + A2($justinmimbs$time_extra$Time$Extra$timeFromPosix, zone, posix);
		return ((localMillis - millis) / 60000) | 0;
	});
var $justinmimbs$time_extra$Time$Extra$posixFromDateTime = F3(
	function (zone, date, time) {
		var millis = $justinmimbs$time_extra$Time$Extra$dateToMillis(date) + time;
		var offset0 = A2(
			$justinmimbs$time_extra$Time$Extra$toOffset,
			zone,
			$elm$time$Time$millisToPosix(millis));
		var posix1 = $elm$time$Time$millisToPosix(millis - (offset0 * 60000));
		var offset1 = A2($justinmimbs$time_extra$Time$Extra$toOffset, zone, posix1);
		if (_Utils_eq(offset0, offset1)) {
			return posix1;
		} else {
			var posix2 = $elm$time$Time$millisToPosix(millis - (offset1 * 60000));
			var offset2 = A2($justinmimbs$time_extra$Time$Extra$toOffset, zone, posix2);
			return _Utils_eq(offset1, offset2) ? posix2 : posix1;
		}
	});
var $justinmimbs$time_extra$Time$Extra$add = F4(
	function (interval, n, zone, posix) {
		add:
		while (true) {
			switch (interval.$) {
				case 'Millisecond':
					return $elm$time$Time$millisToPosix(
						$elm$time$Time$posixToMillis(posix) + n);
				case 'Second':
					var $temp$interval = $justinmimbs$time_extra$Time$Extra$Millisecond,
						$temp$n = n * 1000,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 'Minute':
					var $temp$interval = $justinmimbs$time_extra$Time$Extra$Millisecond,
						$temp$n = n * 60000,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 'Hour':
					var $temp$interval = $justinmimbs$time_extra$Time$Extra$Millisecond,
						$temp$n = n * 3600000,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 'Day':
					return A3(
						$justinmimbs$time_extra$Time$Extra$posixFromDateTime,
						zone,
						A3(
							$justinmimbs$date$Date$add,
							$justinmimbs$date$Date$Days,
							n,
							A2($justinmimbs$date$Date$fromPosix, zone, posix)),
						A2($justinmimbs$time_extra$Time$Extra$timeFromPosix, zone, posix));
				case 'Month':
					return A3(
						$justinmimbs$time_extra$Time$Extra$posixFromDateTime,
						zone,
						A3(
							$justinmimbs$date$Date$add,
							$justinmimbs$date$Date$Months,
							n,
							A2($justinmimbs$date$Date$fromPosix, zone, posix)),
						A2($justinmimbs$time_extra$Time$Extra$timeFromPosix, zone, posix));
				case 'Year':
					var $temp$interval = $justinmimbs$time_extra$Time$Extra$Month,
						$temp$n = n * 12,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 'Quarter':
					var $temp$interval = $justinmimbs$time_extra$Time$Extra$Month,
						$temp$n = n * 3,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 'Week':
					var $temp$interval = $justinmimbs$time_extra$Time$Extra$Day,
						$temp$n = n * 7,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				default:
					var weekday = interval;
					var $temp$interval = $justinmimbs$time_extra$Time$Extra$Day,
						$temp$n = n * 7,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
			}
		}
	});
var $justinmimbs$time_extra$Time$Extra$Week = {$: 'Week'};
var $justinmimbs$date$Date$Day = {$: 'Day'};
var $justinmimbs$date$Date$Friday = {$: 'Friday'};
var $justinmimbs$date$Date$Monday = {$: 'Monday'};
var $justinmimbs$date$Date$Month = {$: 'Month'};
var $justinmimbs$date$Date$Quarter = {$: 'Quarter'};
var $justinmimbs$date$Date$Saturday = {$: 'Saturday'};
var $justinmimbs$date$Date$Sunday = {$: 'Sunday'};
var $justinmimbs$date$Date$Thursday = {$: 'Thursday'};
var $justinmimbs$date$Date$Tuesday = {$: 'Tuesday'};
var $justinmimbs$date$Date$Wednesday = {$: 'Wednesday'};
var $justinmimbs$date$Date$Week = {$: 'Week'};
var $justinmimbs$date$Date$Year = {$: 'Year'};
var $justinmimbs$date$Date$weekdayNumber = function (_v0) {
	var rd = _v0.a;
	var _v1 = A2($elm$core$Basics$modBy, 7, rd);
	if (!_v1) {
		return 7;
	} else {
		var n = _v1;
		return n;
	}
};
var $justinmimbs$date$Date$weekdayToNumber = function (wd) {
	switch (wd.$) {
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
var $justinmimbs$date$Date$daysSincePreviousWeekday = F2(
	function (wd, date) {
		return A2(
			$elm$core$Basics$modBy,
			7,
			($justinmimbs$date$Date$weekdayNumber(date) + 7) - $justinmimbs$date$Date$weekdayToNumber(wd));
	});
var $justinmimbs$date$Date$firstOfMonth = F2(
	function (y, m) {
		return $justinmimbs$date$Date$RD(
			($justinmimbs$date$Date$daysBeforeYear(y) + A2($justinmimbs$date$Date$daysBeforeMonth, y, m)) + 1);
	});
var $justinmimbs$date$Date$firstOfYear = function (y) {
	return $justinmimbs$date$Date$RD(
		$justinmimbs$date$Date$daysBeforeYear(y) + 1);
};
var $justinmimbs$date$Date$month = A2(
	$elm$core$Basics$composeR,
	$justinmimbs$date$Date$toCalendarDate,
	function ($) {
		return $.month;
	});
var $justinmimbs$date$Date$monthToQuarter = function (m) {
	return (($justinmimbs$date$Date$monthToNumber(m) + 2) / 3) | 0;
};
var $justinmimbs$date$Date$quarter = A2($elm$core$Basics$composeR, $justinmimbs$date$Date$month, $justinmimbs$date$Date$monthToQuarter);
var $justinmimbs$date$Date$quarterToMonth = function (q) {
	return $justinmimbs$date$Date$numberToMonth((q * 3) - 2);
};
var $justinmimbs$date$Date$floor = F2(
	function (interval, date) {
		var rd = date.a;
		switch (interval.$) {
			case 'Year':
				return $justinmimbs$date$Date$firstOfYear(
					$justinmimbs$date$Date$year(date));
			case 'Quarter':
				return A2(
					$justinmimbs$date$Date$firstOfMonth,
					$justinmimbs$date$Date$year(date),
					$justinmimbs$date$Date$quarterToMonth(
						$justinmimbs$date$Date$quarter(date)));
			case 'Month':
				return A2(
					$justinmimbs$date$Date$firstOfMonth,
					$justinmimbs$date$Date$year(date),
					$justinmimbs$date$Date$month(date));
			case 'Week':
				return $justinmimbs$date$Date$RD(
					rd - A2($justinmimbs$date$Date$daysSincePreviousWeekday, $elm$time$Time$Mon, date));
			case 'Monday':
				return $justinmimbs$date$Date$RD(
					rd - A2($justinmimbs$date$Date$daysSincePreviousWeekday, $elm$time$Time$Mon, date));
			case 'Tuesday':
				return $justinmimbs$date$Date$RD(
					rd - A2($justinmimbs$date$Date$daysSincePreviousWeekday, $elm$time$Time$Tue, date));
			case 'Wednesday':
				return $justinmimbs$date$Date$RD(
					rd - A2($justinmimbs$date$Date$daysSincePreviousWeekday, $elm$time$Time$Wed, date));
			case 'Thursday':
				return $justinmimbs$date$Date$RD(
					rd - A2($justinmimbs$date$Date$daysSincePreviousWeekday, $elm$time$Time$Thu, date));
			case 'Friday':
				return $justinmimbs$date$Date$RD(
					rd - A2($justinmimbs$date$Date$daysSincePreviousWeekday, $elm$time$Time$Fri, date));
			case 'Saturday':
				return $justinmimbs$date$Date$RD(
					rd - A2($justinmimbs$date$Date$daysSincePreviousWeekday, $elm$time$Time$Sat, date));
			case 'Sunday':
				return $justinmimbs$date$Date$RD(
					rd - A2($justinmimbs$date$Date$daysSincePreviousWeekday, $elm$time$Time$Sun, date));
			default:
				return date;
		}
	});
var $justinmimbs$time_extra$Time$Extra$floorDate = F3(
	function (dateInterval, zone, posix) {
		return A3(
			$justinmimbs$time_extra$Time$Extra$posixFromDateTime,
			zone,
			A2(
				$justinmimbs$date$Date$floor,
				dateInterval,
				A2($justinmimbs$date$Date$fromPosix, zone, posix)),
			0);
	});
var $justinmimbs$time_extra$Time$Extra$floor = F3(
	function (interval, zone, posix) {
		switch (interval.$) {
			case 'Millisecond':
				return posix;
			case 'Second':
				return A3(
					$justinmimbs$time_extra$Time$Extra$posixFromDateTime,
					zone,
					A2($justinmimbs$date$Date$fromPosix, zone, posix),
					A4(
						$justinmimbs$time_extra$Time$Extra$timeFromClock,
						A2($elm$time$Time$toHour, zone, posix),
						A2($elm$time$Time$toMinute, zone, posix),
						A2($elm$time$Time$toSecond, zone, posix),
						0));
			case 'Minute':
				return A3(
					$justinmimbs$time_extra$Time$Extra$posixFromDateTime,
					zone,
					A2($justinmimbs$date$Date$fromPosix, zone, posix),
					A4(
						$justinmimbs$time_extra$Time$Extra$timeFromClock,
						A2($elm$time$Time$toHour, zone, posix),
						A2($elm$time$Time$toMinute, zone, posix),
						0,
						0));
			case 'Hour':
				return A3(
					$justinmimbs$time_extra$Time$Extra$posixFromDateTime,
					zone,
					A2($justinmimbs$date$Date$fromPosix, zone, posix),
					A4(
						$justinmimbs$time_extra$Time$Extra$timeFromClock,
						A2($elm$time$Time$toHour, zone, posix),
						0,
						0,
						0));
			case 'Day':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Day, zone, posix);
			case 'Month':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Month, zone, posix);
			case 'Year':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Year, zone, posix);
			case 'Quarter':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Quarter, zone, posix);
			case 'Week':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Week, zone, posix);
			case 'Monday':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Monday, zone, posix);
			case 'Tuesday':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Tuesday, zone, posix);
			case 'Wednesday':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Wednesday, zone, posix);
			case 'Thursday':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Thursday, zone, posix);
			case 'Friday':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Friday, zone, posix);
			case 'Saturday':
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Saturday, zone, posix);
			default:
				return A3($justinmimbs$time_extra$Time$Extra$floorDate, $justinmimbs$date$Date$Sunday, zone, posix);
		}
	});
var $justinmimbs$time_extra$Time$Extra$ceiling = F3(
	function (interval, zone, posix) {
		var floored = A3($justinmimbs$time_extra$Time$Extra$floor, interval, zone, posix);
		return _Utils_eq(floored, posix) ? posix : A4($justinmimbs$time_extra$Time$Extra$add, interval, 1, zone, floored);
	});
var $terezka$intervals$Intervals$Time$ceilingDay = F3(
	function (zone, mult, stamp) {
		return (mult === 7) ? A3($justinmimbs$time_extra$Time$Extra$ceiling, $justinmimbs$time_extra$Time$Extra$Week, zone, stamp) : A3($justinmimbs$time_extra$Time$Extra$ceiling, $justinmimbs$time_extra$Time$Extra$Day, zone, stamp);
	});
var $justinmimbs$time_extra$Time$Extra$Hour = {$: 'Hour'};
var $justinmimbs$time_extra$Time$Extra$partsToPosix = F2(
	function (zone, _v0) {
		var year = _v0.year;
		var month = _v0.month;
		var day = _v0.day;
		var hour = _v0.hour;
		var minute = _v0.minute;
		var second = _v0.second;
		var millisecond = _v0.millisecond;
		return A3(
			$justinmimbs$time_extra$Time$Extra$posixFromDateTime,
			zone,
			A3($justinmimbs$date$Date$fromCalendarDate, year, month, day),
			A4(
				$justinmimbs$time_extra$Time$Extra$timeFromClock,
				A3($elm$core$Basics$clamp, 0, 23, hour),
				A3($elm$core$Basics$clamp, 0, 59, minute),
				A3($elm$core$Basics$clamp, 0, 59, second),
				A3($elm$core$Basics$clamp, 0, 999, millisecond)));
	});
var $justinmimbs$time_extra$Time$Extra$posixToParts = F2(
	function (zone, posix) {
		return {
			day: A2($elm$time$Time$toDay, zone, posix),
			hour: A2($elm$time$Time$toHour, zone, posix),
			millisecond: A2($elm$time$Time$toMillis, zone, posix),
			minute: A2($elm$time$Time$toMinute, zone, posix),
			month: A2($elm$time$Time$toMonth, zone, posix),
			second: A2($elm$time$Time$toSecond, zone, posix),
			year: A2($elm$time$Time$toYear, zone, posix)
		};
	});
var $terezka$intervals$Intervals$Time$ceilingHour = F3(
	function (zone, mult, stamp) {
		var parts = A2(
			$justinmimbs$time_extra$Time$Extra$posixToParts,
			zone,
			A3($justinmimbs$time_extra$Time$Extra$ceiling, $justinmimbs$time_extra$Time$Extra$Hour, zone, stamp));
		var rem = parts.hour % mult;
		return (!rem) ? A2($justinmimbs$time_extra$Time$Extra$partsToPosix, zone, parts) : A4($justinmimbs$time_extra$Time$Extra$add, $justinmimbs$time_extra$Time$Extra$Hour, mult - rem, zone, stamp);
	});
var $justinmimbs$time_extra$Time$Extra$Minute = {$: 'Minute'};
var $terezka$intervals$Intervals$Time$ceilingMinute = F3(
	function (zone, mult, stamp) {
		var parts = A2(
			$justinmimbs$time_extra$Time$Extra$posixToParts,
			zone,
			A3($justinmimbs$time_extra$Time$Extra$ceiling, $justinmimbs$time_extra$Time$Extra$Minute, zone, stamp));
		var rem = parts.minute % mult;
		return (!rem) ? A2($justinmimbs$time_extra$Time$Extra$partsToPosix, zone, parts) : A4($justinmimbs$time_extra$Time$Extra$add, $justinmimbs$time_extra$Time$Extra$Minute, mult - rem, zone, stamp);
	});
var $terezka$intervals$Intervals$Time$intAsMonth = function (_int) {
	switch (_int) {
		case 1:
			return $elm$time$Time$Jan;
		case 2:
			return $elm$time$Time$Feb;
		case 3:
			return $elm$time$Time$Mar;
		case 4:
			return $elm$time$Time$Apr;
		case 5:
			return $elm$time$Time$May;
		case 6:
			return $elm$time$Time$Jun;
		case 7:
			return $elm$time$Time$Jul;
		case 8:
			return $elm$time$Time$Aug;
		case 9:
			return $elm$time$Time$Sep;
		case 10:
			return $elm$time$Time$Oct;
		case 11:
			return $elm$time$Time$Nov;
		case 12:
			return $elm$time$Time$Dec;
		default:
			return $elm$time$Time$Dec;
	}
};
var $terezka$intervals$Intervals$Time$monthAsInt = function (month) {
	switch (month.$) {
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
var $terezka$intervals$Intervals$Time$ceilingMonth = F3(
	function (zone, mult, stamp) {
		var parts = A2(
			$justinmimbs$time_extra$Time$Extra$posixToParts,
			zone,
			A3($justinmimbs$time_extra$Time$Extra$ceiling, $justinmimbs$time_extra$Time$Extra$Month, zone, stamp));
		var monthInt = $terezka$intervals$Intervals$Time$monthAsInt(parts.month);
		var rem = (monthInt - 1) % mult;
		var newMonth = (!rem) ? monthInt : ((monthInt - rem) + mult);
		return A2(
			$justinmimbs$time_extra$Time$Extra$partsToPosix,
			zone,
			(newMonth > 12) ? _Utils_update(
				parts,
				{
					month: $terezka$intervals$Intervals$Time$intAsMonth(newMonth - 12),
					year: parts.year + 1
				}) : _Utils_update(
				parts,
				{
					month: $terezka$intervals$Intervals$Time$intAsMonth(newMonth)
				}));
	});
var $terezka$intervals$Intervals$Time$ceilingMs = F3(
	function (zone, mult, stamp) {
		var parts = A2($justinmimbs$time_extra$Time$Extra$posixToParts, zone, stamp);
		var rem = parts.millisecond % mult;
		return (!rem) ? A2($justinmimbs$time_extra$Time$Extra$partsToPosix, zone, parts) : A4($justinmimbs$time_extra$Time$Extra$add, $justinmimbs$time_extra$Time$Extra$Millisecond, mult - rem, zone, stamp);
	});
var $justinmimbs$time_extra$Time$Extra$Second = {$: 'Second'};
var $terezka$intervals$Intervals$Time$ceilingSecond = F3(
	function (zone, mult, stamp) {
		var parts = A2(
			$justinmimbs$time_extra$Time$Extra$posixToParts,
			zone,
			A3($justinmimbs$time_extra$Time$Extra$ceiling, $justinmimbs$time_extra$Time$Extra$Second, zone, stamp));
		var rem = parts.second % mult;
		return (!rem) ? A2($justinmimbs$time_extra$Time$Extra$partsToPosix, zone, parts) : A4($justinmimbs$time_extra$Time$Extra$add, $justinmimbs$time_extra$Time$Extra$Second, mult - rem, zone, stamp);
	});
var $justinmimbs$time_extra$Time$Extra$Year = {$: 'Year'};
var $terezka$intervals$Intervals$Time$ceilingYear = F3(
	function (zone, mult, stamp) {
		var parts = A2(
			$justinmimbs$time_extra$Time$Extra$posixToParts,
			zone,
			A3($justinmimbs$time_extra$Time$Extra$ceiling, $justinmimbs$time_extra$Time$Extra$Year, zone, stamp));
		var rem = parts.year % mult;
		var newYear = (!rem) ? parts.year : ((parts.year - rem) + mult);
		return A2(
			$justinmimbs$time_extra$Time$Extra$partsToPosix,
			zone,
			_Utils_update(
				parts,
				{year: newYear}));
	});
var $terezka$intervals$Intervals$Time$ceilingUnit = F3(
	function (zone, unit, mult) {
		switch (unit.$) {
			case 'Millisecond':
				return A2($terezka$intervals$Intervals$Time$ceilingMs, zone, mult);
			case 'Second':
				return A2($terezka$intervals$Intervals$Time$ceilingSecond, zone, mult);
			case 'Minute':
				return A2($terezka$intervals$Intervals$Time$ceilingMinute, zone, mult);
			case 'Hour':
				return A2($terezka$intervals$Intervals$Time$ceilingHour, zone, mult);
			case 'Day':
				return A2($terezka$intervals$Intervals$Time$ceilingDay, zone, mult);
			case 'Month':
				return A2($terezka$intervals$Intervals$Time$ceilingMonth, zone, mult);
			default:
				return A2($terezka$intervals$Intervals$Time$ceilingYear, zone, mult);
		}
	});
var $terezka$intervals$Intervals$Time$Day = {$: 'Day'};
var $terezka$intervals$Intervals$Time$Hour = {$: 'Hour'};
var $terezka$intervals$Intervals$Time$Millisecond = {$: 'Millisecond'};
var $terezka$intervals$Intervals$Time$Minute = {$: 'Minute'};
var $terezka$intervals$Intervals$Time$Month = {$: 'Month'};
var $terezka$intervals$Intervals$Time$Second = {$: 'Second'};
var $terezka$intervals$Intervals$Time$Year = {$: 'Year'};
var $terezka$intervals$Intervals$Time$getChange = F3(
	function (zone, a, b) {
		var bP = A2($justinmimbs$time_extra$Time$Extra$posixToParts, zone, b);
		var aP = A2($justinmimbs$time_extra$Time$Extra$posixToParts, zone, a);
		return (!_Utils_eq(aP.year, bP.year)) ? $terezka$intervals$Intervals$Time$Year : ((!_Utils_eq(aP.month, bP.month)) ? $terezka$intervals$Intervals$Time$Month : ((!_Utils_eq(aP.day, bP.day)) ? $terezka$intervals$Intervals$Time$Day : ((!_Utils_eq(aP.hour, bP.hour)) ? $terezka$intervals$Intervals$Time$Hour : ((!_Utils_eq(aP.minute, bP.minute)) ? $terezka$intervals$Intervals$Time$Minute : ((!_Utils_eq(aP.second, bP.second)) ? $terezka$intervals$Intervals$Time$Second : $terezka$intervals$Intervals$Time$Millisecond)))));
	});
var $danhandrea$elm_time_extra$Util$isLeapYear = function (year) {
	return (!A2($elm$core$Basics$modBy, 400, year)) || ((!(!A2($elm$core$Basics$modBy, 100, year))) && (!A2($elm$core$Basics$modBy, 4, year)));
};
var $danhandrea$elm_time_extra$Month$days = F2(
	function (year, month) {
		switch (month.$) {
			case 'Jan':
				return 31;
			case 'Feb':
				return $danhandrea$elm_time_extra$Util$isLeapYear(year) ? 29 : 28;
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
var $danhandrea$elm_time_extra$TimeExtra$daysInMonth = $danhandrea$elm_time_extra$Month$days;
var $terezka$intervals$Intervals$Time$toMs = $elm$time$Time$posixToMillis;
var $terezka$intervals$Intervals$Time$getDiff = F3(
	function (zone, a, b) {
		var _v0 = (_Utils_cmp(
			$terezka$intervals$Intervals$Time$toMs(a),
			$terezka$intervals$Intervals$Time$toMs(b)) < 0) ? _Utils_Tuple2(
			A2($justinmimbs$time_extra$Time$Extra$posixToParts, zone, a),
			A2($justinmimbs$time_extra$Time$Extra$posixToParts, zone, b)) : _Utils_Tuple2(
			A2($justinmimbs$time_extra$Time$Extra$posixToParts, zone, b),
			A2($justinmimbs$time_extra$Time$Extra$posixToParts, zone, a));
		var aP = _v0.a;
		var bP = _v0.b;
		var dMsX = bP.millisecond - aP.millisecond;
		var dMs = (dMsX < 0) ? (1000 + dMsX) : dMsX;
		var dSecondX = (bP.second - aP.second) + ((dMsX < 0) ? (-1) : 0);
		var dMinuteX = (bP.minute - aP.minute) + ((dSecondX < 0) ? (-1) : 0);
		var dHourX = (bP.hour - aP.hour) + ((dMinuteX < 0) ? (-1) : 0);
		var dDayX = (bP.day - aP.day) + ((dHourX < 0) ? (-1) : 0);
		var dDay = (dDayX < 0) ? (A2($danhandrea$elm_time_extra$TimeExtra$daysInMonth, bP.year, bP.month) + dDayX) : dDayX;
		var dMonthX = ($terezka$intervals$Intervals$Time$monthAsInt(bP.month) - $terezka$intervals$Intervals$Time$monthAsInt(aP.month)) + ((dDayX < 0) ? (-1) : 0);
		var dMonth = (dMonthX < 0) ? (12 + dMonthX) : dMonthX;
		var dHour = (dHourX < 0) ? (24 + dHourX) : dHourX;
		var dMinute = (dMinuteX < 0) ? (60 + dMinuteX) : dMinuteX;
		var dSecond = (dSecondX < 0) ? (60 + dSecondX) : dSecondX;
		var dYearX = (bP.year - aP.year) + ((dMonthX < 0) ? (-1) : 0);
		var dYear = (dYearX < 0) ? ($terezka$intervals$Intervals$Time$monthAsInt(bP.month) + dYearX) : dYearX;
		return {day: dDay, hour: dHour, millisecond: dMs, minute: dMinute, month: dMonth, second: dSecond, year: dYear};
	});
var $terezka$intervals$Intervals$Time$oneSecond = 1000;
var $terezka$intervals$Intervals$Time$oneMinute = $terezka$intervals$Intervals$Time$oneSecond * 60;
var $terezka$intervals$Intervals$Time$oneHour = $terezka$intervals$Intervals$Time$oneMinute * 60;
var $terezka$intervals$Intervals$Time$oneDay = $terezka$intervals$Intervals$Time$oneHour * 24;
var $terezka$intervals$Intervals$Time$oneMs = 1;
var $terezka$intervals$Intervals$Time$getNumOfTicks = F5(
	function (zone, unit, mult, a, b) {
		var div = F2(
			function (n1, n2) {
				return $elm$core$Basics$floor(n1 / n2);
			});
		var timeDiff = function (ms) {
			var ceiled = A4($terezka$intervals$Intervals$Time$ceilingUnit, zone, unit, mult, a);
			return (_Utils_cmp(
				$terezka$intervals$Intervals$Time$toMs(ceiled),
				$terezka$intervals$Intervals$Time$toMs(b)) > 0) ? (-1) : A2(
				div,
				A2(
					div,
					$terezka$intervals$Intervals$Time$toMs(b) - $terezka$intervals$Intervals$Time$toMs(ceiled),
					ms),
				mult);
		};
		var diff = function (property) {
			var ceiled = A4($terezka$intervals$Intervals$Time$ceilingUnit, zone, unit, mult, a);
			return (_Utils_cmp(
				$terezka$intervals$Intervals$Time$toMs(ceiled),
				$terezka$intervals$Intervals$Time$toMs(b)) > 0) ? (-1) : A2(
				div,
				property(
					A3($terezka$intervals$Intervals$Time$getDiff, zone, ceiled, b)),
				mult);
		};
		switch (unit.$) {
			case 'Millisecond':
				return timeDiff($terezka$intervals$Intervals$Time$oneMs) + 1;
			case 'Second':
				return timeDiff($terezka$intervals$Intervals$Time$oneSecond) + 1;
			case 'Minute':
				return timeDiff($terezka$intervals$Intervals$Time$oneMinute) + 1;
			case 'Hour':
				return timeDiff($terezka$intervals$Intervals$Time$oneHour) + 1;
			case 'Day':
				return timeDiff($terezka$intervals$Intervals$Time$oneDay) + 1;
			case 'Month':
				return diff(
					function (d) {
						return d.month + (d.year * 12);
					}) + 1;
			default:
				return diff(
					function ($) {
						return $.year;
					}) + 1;
		}
	});
var $terezka$intervals$Intervals$Time$largerUnit = function (unit) {
	switch (unit.$) {
		case 'Millisecond':
			return $elm$core$Maybe$Just($terezka$intervals$Intervals$Time$Second);
		case 'Second':
			return $elm$core$Maybe$Just($terezka$intervals$Intervals$Time$Minute);
		case 'Minute':
			return $elm$core$Maybe$Just($terezka$intervals$Intervals$Time$Hour);
		case 'Hour':
			return $elm$core$Maybe$Just($terezka$intervals$Intervals$Time$Day);
		case 'Day':
			return $elm$core$Maybe$Just($terezka$intervals$Intervals$Time$Month);
		case 'Month':
			return $elm$core$Maybe$Just($terezka$intervals$Intervals$Time$Year);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $terezka$intervals$Intervals$Time$niceMultiples = function (unit) {
	switch (unit.$) {
		case 'Millisecond':
			return _List_fromArray(
				[1, 2, 5, 10, 20, 25, 50, 100, 200, 500]);
		case 'Second':
			return _List_fromArray(
				[1, 2, 5, 10, 15, 30]);
		case 'Minute':
			return _List_fromArray(
				[1, 2, 5, 10, 15, 30]);
		case 'Hour':
			return _List_fromArray(
				[1, 2, 3, 4, 6, 8, 12]);
		case 'Day':
			return _List_fromArray(
				[1, 2, 3, 7, 14]);
		case 'Month':
			return _List_fromArray(
				[1, 2, 3, 4, 6]);
		default:
			return _List_fromArray(
				[1, 2, 5, 10, 20, 25, 50, 100, 200, 500, 1000, 10000, 1000000, 10000000]);
	}
};
var $terezka$intervals$Intervals$Time$toBestUnit = F4(
	function (zone, amount, min, max) {
		var toNice = function (unit) {
			toNice:
			while (true) {
				var niceNums = $terezka$intervals$Intervals$Time$niceMultiples(unit);
				var maybeNiceNum = A2(
					$elm$core$List$filter,
					function (n) {
						return _Utils_cmp(
							A5($terezka$intervals$Intervals$Time$getNumOfTicks, zone, unit, n, min, max),
							amount) < 1;
					},
					niceNums);
				var div = F2(
					function (n1, n2) {
						return $elm$core$Basics$ceiling(n1 / n2);
					});
				var _v0 = $elm$core$List$head(maybeNiceNum);
				if (_v0.$ === 'Just') {
					var niceNum = _v0.a;
					return _Utils_Tuple2(unit, niceNum);
				} else {
					var _v1 = $terezka$intervals$Intervals$Time$largerUnit(unit);
					if (_v1.$ === 'Just') {
						var larger = _v1.a;
						var $temp$unit = larger;
						unit = $temp$unit;
						continue toNice;
					} else {
						return _Utils_Tuple2($terezka$intervals$Intervals$Time$Year, 100000000);
					}
				}
			}
		};
		return toNice($terezka$intervals$Intervals$Time$Millisecond);
	});
var $terezka$intervals$Intervals$Time$toExtraUnit = function (unit) {
	switch (unit.$) {
		case 'Millisecond':
			return $justinmimbs$time_extra$Time$Extra$Millisecond;
		case 'Second':
			return $justinmimbs$time_extra$Time$Extra$Second;
		case 'Minute':
			return $justinmimbs$time_extra$Time$Extra$Minute;
		case 'Hour':
			return $justinmimbs$time_extra$Time$Extra$Hour;
		case 'Day':
			return $justinmimbs$time_extra$Time$Extra$Day;
		case 'Month':
			return $justinmimbs$time_extra$Time$Extra$Month;
		default:
			return $justinmimbs$time_extra$Time$Extra$Year;
	}
};
var $terezka$intervals$Intervals$Time$unitToInt = function (unit) {
	switch (unit.$) {
		case 'Millisecond':
			return 0;
		case 'Second':
			return 1;
		case 'Minute':
			return 2;
		case 'Hour':
			return 3;
		case 'Day':
			return 4;
		case 'Month':
			return 5;
		default:
			return 6;
	}
};
var $terezka$intervals$Intervals$Time$values = F4(
	function (zone, maxMmount, min, max) {
		var _v0 = A4($terezka$intervals$Intervals$Time$toBestUnit, zone, maxMmount, min, max);
		var unit = _v0.a;
		var mult = _v0.b;
		var amount = A5($terezka$intervals$Intervals$Time$getNumOfTicks, zone, unit, mult, min, max);
		var initial = A4($terezka$intervals$Intervals$Time$ceilingUnit, zone, unit, mult, min);
		var tUnit = $terezka$intervals$Intervals$Time$toExtraUnit(unit);
		var toTick = F3(
			function (x, timestamp, change) {
				return {
					change: (_Utils_cmp(
						$terezka$intervals$Intervals$Time$unitToInt(change),
						$terezka$intervals$Intervals$Time$unitToInt(unit)) > 0) ? $elm$core$Maybe$Just(change) : $elm$core$Maybe$Nothing,
					isFirst: !x,
					multiple: mult,
					timestamp: timestamp,
					unit: unit,
					zone: zone
				};
			});
		var toTicks = F2(
			function (xs, acc) {
				toTicks:
				while (true) {
					if (xs.b) {
						var x = xs.a;
						var rest = xs.b;
						var prev = A4($justinmimbs$time_extra$Time$Extra$add, tUnit, (x - 1) * mult, zone, initial);
						var curr = A4($justinmimbs$time_extra$Time$Extra$add, tUnit, x * mult, zone, initial);
						var change = A3($terezka$intervals$Intervals$Time$getChange, zone, prev, curr);
						var $temp$xs = rest,
							$temp$acc = A2(
							$elm$core$List$cons,
							A3(toTick, x, curr, change),
							acc);
						xs = $temp$xs;
						acc = $temp$acc;
						continue toTicks;
					} else {
						return acc;
					}
				}
			});
		return $elm$core$List$reverse(
			A2(
				toTicks,
				A2($elm$core$List$range, 0, amount - 1),
				_List_Nil));
	});
var $terezka$intervals$Intervals$times = F3(
	function (zone, amount, range) {
		var toUnit = function (unit) {
			switch (unit.$) {
				case 'Millisecond':
					return $terezka$intervals$Intervals$Millisecond;
				case 'Second':
					return $terezka$intervals$Intervals$Second;
				case 'Minute':
					return $terezka$intervals$Intervals$Minute;
				case 'Hour':
					return $terezka$intervals$Intervals$Hour;
				case 'Day':
					return $terezka$intervals$Intervals$Day;
				case 'Month':
					return $terezka$intervals$Intervals$Month;
				default:
					return $terezka$intervals$Intervals$Year;
			}
		};
		var translateUnit = function (time) {
			return {
				change: A2($elm$core$Maybe$map, toUnit, time.change),
				isFirst: time.isFirst,
				multiple: time.multiple,
				timestamp: time.timestamp,
				unit: toUnit(time.unit),
				zone: time.zone
			};
		};
		var fromMs = function (ts) {
			return $elm$time$Time$millisToPosix(
				$elm$core$Basics$round(ts));
		};
		return A2(
			$elm$core$List$map,
			translateUnit,
			A4(
				$terezka$intervals$Intervals$Time$values,
				zone,
				amount,
				fromMs(range.min),
				fromMs(range.max)));
	});
var $terezka$elm_charts$Internal$Svg$times = function (zone) {
	return $terezka$elm_charts$Internal$Svg$Generator(
		F2(
			function (i, b) {
				return A3(
					$terezka$intervals$Intervals$times,
					zone,
					i,
					{max: b.max, min: b.min});
			}));
};
var $terezka$elm_charts$Chart$Svg$times = $terezka$elm_charts$Internal$Svg$times;
var $terezka$elm_charts$Chart$generateValues = F4(
	function (amount, tick, maybeFormat, axis) {
		var toTickValues = F2(
			function (toValue, toString) {
				return $elm$core$List$map(
					function (i) {
						return {
							label: function () {
								if (maybeFormat.$ === 'Just') {
									var formatter = maybeFormat.a;
									return formatter(
										toValue(i));
								} else {
									return toString(i);
								}
							}(),
							value: toValue(i)
						};
					});
			});
		switch (tick.$) {
			case 'Floats':
				return A3(
					toTickValues,
					$elm$core$Basics$identity,
					$elm$core$String$fromFloat,
					A3($terezka$elm_charts$Chart$Svg$generate, amount, $terezka$elm_charts$Chart$Svg$floats, axis));
			case 'Ints':
				return A3(
					toTickValues,
					$elm$core$Basics$toFloat,
					$elm$core$String$fromInt,
					A3($terezka$elm_charts$Chart$Svg$generate, amount, $terezka$elm_charts$Chart$Svg$ints, axis));
			default:
				var zone = tick.a;
				return A3(
					toTickValues,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Basics$toFloat, $elm$time$Time$posixToMillis),
						function ($) {
							return $.timestamp;
						}),
					$terezka$elm_charts$Chart$Svg$formatTime(zone),
					A3(
						$terezka$elm_charts$Chart$Svg$generate,
						amount,
						$terezka$elm_charts$Chart$Svg$times(zone),
						axis));
		}
	});
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $elm$svg$Svg$tspan = $elm$svg$Svg$trustedNode('tspan');
var $terezka$elm_charts$Internal$Svg$label = F4(
	function (plane, config, inner, point) {
		var uppercaseStyle = config.uppercase ? 'text-transform: uppercase;' : '';
		var fontStyle = function () {
			var _v4 = config.fontSize;
			if (_v4.$ === 'Just') {
				var size_ = _v4.a;
				return 'font-size: ' + ($elm$core$String$fromInt(size_) + 'px;');
			} else {
				return '';
			}
		}();
		var anchorStyle = function () {
			var _v0 = config.anchor;
			if (_v0.$ === 'Nothing') {
				return 'text-anchor: middle;';
			} else {
				switch (_v0.a.$) {
					case 'End':
						var _v1 = _v0.a;
						return 'text-anchor: end;';
					case 'Start':
						var _v2 = _v0.a;
						return 'text-anchor: start;';
					default:
						var _v3 = _v0.a;
						return 'text-anchor: middle;';
				}
			}
		}();
		return A4(
			$terezka$elm_charts$Internal$Svg$withAttrs,
			config.attrs,
			$elm$svg$Svg$text_,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('elm-charts__label'),
					$elm$svg$Svg$Attributes$stroke(config.border),
					$elm$svg$Svg$Attributes$strokeWidth(
					$elm$core$String$fromFloat(config.borderWidth)),
					$elm$svg$Svg$Attributes$fill(config.color),
					A6($terezka$elm_charts$Internal$Svg$position, plane, -config.rotate, point.x, point.y, config.xOff, config.yOff),
					$elm$svg$Svg$Attributes$style(
					A2(
						$elm$core$String$join,
						' ',
						_List_fromArray(
							['pointer-events: none;', fontStyle, anchorStyle, uppercaseStyle])))
				]),
			_List_fromArray(
				[
					A2($elm$svg$Svg$tspan, _List_Nil, inner)
				]));
	});
var $terezka$elm_charts$Chart$xLabels = function (edits) {
	var toTicks = F2(
		function (p, config) {
			return A4(
				$terezka$elm_charts$Chart$generateValues,
				config.amount,
				config.generate,
				config.format,
				A3(
					$elm$core$List$foldl,
					F2(
						function (f, x) {
							return f(x);
						}),
					p.x,
					config.limits));
		});
	var toTickValues = F3(
		function (p, config, ts) {
			return (!config.grid) ? ts : _Utils_update(
				ts,
				{
					xs: _Utils_ap(
						ts.xs,
						A2(
							$elm$core$List$map,
							function ($) {
								return $.value;
							},
							A2(toTicks, p, config)))
				});
		});
	var toConfig = function (p) {
		return A2(
			$terezka$elm_charts$Internal$Helpers$apply,
			edits,
			{amount: 5, anchor: $elm$core$Maybe$Nothing, color: '#808BAB', flip: false, fontSize: $elm$core$Maybe$Nothing, format: $elm$core$Maybe$Nothing, generate: $terezka$elm_charts$Internal$Svg$Floats, grid: false, limits: _List_Nil, pinned: $terezka$elm_charts$Chart$Attributes$zero, rotate: 0, uppercase: false, xOff: 0, yOff: 18});
	};
	return A3(
		$terezka$elm_charts$Chart$LabelsElement,
		toConfig,
		toTickValues,
		F2(
			function (p, config) {
				var _default = $terezka$elm_charts$Internal$Svg$defaultLabel;
				var toLabel = function (item) {
					return A4(
						$terezka$elm_charts$Internal$Svg$label,
						p,
						_Utils_update(
							_default,
							{
								anchor: config.anchor,
								color: config.color,
								fontSize: config.fontSize,
								rotate: config.rotate,
								uppercase: config.uppercase,
								xOff: config.xOff,
								yOff: config.flip ? ((-config.yOff) + 10) : config.yOff
							}),
						_List_fromArray(
							[
								$elm$svg$Svg$text(item.label)
							]),
						{
							x: item.value,
							y: config.pinned(p.y)
						});
				};
				return A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('elm-charts__x-labels')
						]),
					A2(
						$elm$core$List$map,
						toLabel,
						A2(toTicks, p, config)));
			}));
};
var $terezka$elm_charts$Chart$TicksElement = F2(
	function (a, b) {
		return {$: 'TicksElement', a: a, b: b};
	});
var $terezka$elm_charts$Chart$Attributes$length = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{length: v});
	});
var $terezka$elm_charts$Internal$Svg$defaultTick = {attrs: _List_Nil, color: 'rgb(210, 210, 210)', length: 5, width: 1};
var $terezka$elm_charts$Internal$Svg$tick = F4(
	function (plane, config, isX, point) {
		return A4(
			$terezka$elm_charts$Internal$Svg$withAttrs,
			config.attrs,
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$class('elm-charts__tick'),
					$elm$svg$Svg$Attributes$stroke(config.color),
					$elm$svg$Svg$Attributes$strokeWidth(
					$elm$core$String$fromFloat(config.width)),
					$elm$svg$Svg$Attributes$x1(
					$elm$core$String$fromFloat(
						A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, point.x))),
					$elm$svg$Svg$Attributes$x2(
					$elm$core$String$fromFloat(
						A2($terezka$elm_charts$Internal$Coordinates$toSVGX, plane, point.x) + (isX ? 0 : (-config.length)))),
					$elm$svg$Svg$Attributes$y1(
					$elm$core$String$fromFloat(
						A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, point.y))),
					$elm$svg$Svg$Attributes$y2(
					$elm$core$String$fromFloat(
						A2($terezka$elm_charts$Internal$Coordinates$toSVGY, plane, point.y) + (isX ? config.length : 0)))
				]),
			_List_Nil);
	});
var $terezka$elm_charts$Internal$Svg$xTick = F3(
	function (plane, config, point) {
		return A4($terezka$elm_charts$Internal$Svg$tick, plane, config, true, point);
	});
var $terezka$elm_charts$Chart$Svg$xTick = F2(
	function (plane, edits) {
		return A2(
			$terezka$elm_charts$Internal$Svg$xTick,
			plane,
			A2($terezka$elm_charts$Internal$Helpers$apply, edits, $terezka$elm_charts$Internal$Svg$defaultTick));
	});
var $terezka$elm_charts$Chart$xTicks = function (edits) {
	var config = A2(
		$terezka$elm_charts$Internal$Helpers$apply,
		edits,
		{amount: 5, color: '', flip: false, generate: $terezka$elm_charts$Internal$Svg$Floats, grid: true, height: 5, limits: _List_Nil, pinned: $terezka$elm_charts$Chart$Attributes$zero, width: 1});
	var toTicks = function (p) {
		return A2(
			$elm$core$List$map,
			function ($) {
				return $.value;
			},
			A4(
				$terezka$elm_charts$Chart$generateValues,
				config.amount,
				config.generate,
				$elm$core$Maybe$Nothing,
				A3(
					$elm$core$List$foldl,
					F2(
						function (f, x) {
							return f(x);
						}),
					p.x,
					config.limits)));
	};
	var addTickValues = F2(
		function (p, ts) {
			return (!config.grid) ? ts : _Utils_update(
				ts,
				{
					xs: _Utils_ap(
						ts.xs,
						toTicks(p))
				});
		});
	return A2(
		$terezka$elm_charts$Chart$TicksElement,
		addTickValues,
		function (p) {
			var toTick = function (x) {
				return A3(
					$terezka$elm_charts$Chart$Svg$xTick,
					p,
					_List_fromArray(
						[
							$terezka$elm_charts$Chart$Attributes$color(config.color),
							$terezka$elm_charts$Chart$Attributes$length(
							config.flip ? (-config.height) : config.height),
							$terezka$elm_charts$Chart$Attributes$width(config.width)
						]),
					{
						x: x,
						y: config.pinned(p.y)
					});
			};
			return A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('elm-charts__x-ticks')
					]),
				A2(
					$elm$core$List$map,
					toTick,
					toTicks(p)));
		});
};
var $terezka$elm_charts$Chart$Attributes$rotate = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{rotate: config.rotate + v});
	});
var $terezka$elm_charts$Chart$Attributes$y2 = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{
				y2: $elm$core$Maybe$Just(v)
			});
	});
var $terezka$elm_charts$Chart$yAxis = function (edits) {
	var config = A2(
		$terezka$elm_charts$Internal$Helpers$apply,
		edits,
		{arrow: true, color: '', limits: _List_Nil, pinned: $terezka$elm_charts$Chart$Attributes$zero, width: 1});
	var addTickValues = F2(
		function (p, ts) {
			return _Utils_update(
				ts,
				{
					xAxis: A2(
						$elm$core$List$cons,
						config.pinned(p.x),
						ts.xAxis)
				});
		});
	return A2(
		$terezka$elm_charts$Chart$AxisElement,
		addTickValues,
		function (p) {
			var yLimit = A3(
				$elm$core$List$foldl,
				F2(
					function (f, y) {
						return f(y);
					}),
				p.y,
				config.limits);
			return A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('elm-charts__y-axis')
					]),
				_List_fromArray(
					[
						A2(
						$terezka$elm_charts$Chart$Svg$line,
						p,
						_List_fromArray(
							[
								$terezka$elm_charts$Chart$Attributes$color(config.color),
								$terezka$elm_charts$Chart$Attributes$width(config.width),
								$terezka$elm_charts$Chart$Attributes$x1(
								config.pinned(p.x)),
								$terezka$elm_charts$Chart$Attributes$y1(
								A2($elm$core$Basics$max, p.y.min, yLimit.min)),
								$terezka$elm_charts$Chart$Attributes$y2(
								A2($elm$core$Basics$min, p.y.max, yLimit.max))
							])),
						config.arrow ? A3(
						$terezka$elm_charts$Chart$Svg$arrow,
						p,
						_List_fromArray(
							[
								$terezka$elm_charts$Chart$Attributes$color(config.color),
								$terezka$elm_charts$Chart$Attributes$rotate(-90)
							]),
						{
							x: config.pinned(p.x),
							y: yLimit.max
						}) : $elm$svg$Svg$text('')
					]));
		});
};
var $terezka$elm_charts$Internal$Svg$End = {$: 'End'};
var $terezka$elm_charts$Chart$yLabels = function (edits) {
	var toTicks = F2(
		function (p, config) {
			return A4(
				$terezka$elm_charts$Chart$generateValues,
				config.amount,
				config.generate,
				config.format,
				A3(
					$elm$core$List$foldl,
					F2(
						function (f, y) {
							return f(y);
						}),
					p.y,
					config.limits));
		});
	var toTickValues = F3(
		function (p, config, ts) {
			return (!config.grid) ? ts : _Utils_update(
				ts,
				{
					ys: _Utils_ap(
						ts.ys,
						A2(
							$elm$core$List$map,
							function ($) {
								return $.value;
							},
							A2(toTicks, p, config)))
				});
		});
	var toConfig = function (p) {
		return A2(
			$terezka$elm_charts$Internal$Helpers$apply,
			edits,
			{amount: 5, anchor: $elm$core$Maybe$Nothing, color: '#808BAB', flip: false, fontSize: $elm$core$Maybe$Nothing, format: $elm$core$Maybe$Nothing, generate: $terezka$elm_charts$Internal$Svg$Floats, grid: false, limits: _List_Nil, pinned: $terezka$elm_charts$Chart$Attributes$zero, rotate: 0, uppercase: false, xOff: -10, yOff: 3});
	};
	return A3(
		$terezka$elm_charts$Chart$LabelsElement,
		toConfig,
		toTickValues,
		F2(
			function (p, config) {
				var _default = $terezka$elm_charts$Internal$Svg$defaultLabel;
				var toLabel = function (item) {
					return A4(
						$terezka$elm_charts$Internal$Svg$label,
						p,
						_Utils_update(
							_default,
							{
								anchor: function () {
									var _v0 = config.anchor;
									if (_v0.$ === 'Nothing') {
										return $elm$core$Maybe$Just(
											config.flip ? $terezka$elm_charts$Internal$Svg$Start : $terezka$elm_charts$Internal$Svg$End);
									} else {
										var anchor = _v0.a;
										return $elm$core$Maybe$Just(anchor);
									}
								}(),
								color: config.color,
								fontSize: config.fontSize,
								rotate: config.rotate,
								uppercase: config.uppercase,
								xOff: config.flip ? (-config.xOff) : config.xOff,
								yOff: config.yOff
							}),
						_List_fromArray(
							[
								$elm$svg$Svg$text(item.label)
							]),
						{
							x: config.pinned(p.x),
							y: item.value
						});
				};
				return A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$class('elm-charts__y-labels')
						]),
					A2(
						$elm$core$List$map,
						toLabel,
						A2(toTicks, p, config)));
			}));
};
var $terezka$elm_charts$Internal$Svg$yTick = F3(
	function (plane, config, point) {
		return A4($terezka$elm_charts$Internal$Svg$tick, plane, config, false, point);
	});
var $terezka$elm_charts$Chart$Svg$yTick = F2(
	function (plane, edits) {
		return A2(
			$terezka$elm_charts$Internal$Svg$yTick,
			plane,
			A2($terezka$elm_charts$Internal$Helpers$apply, edits, $terezka$elm_charts$Internal$Svg$defaultTick));
	});
var $terezka$elm_charts$Chart$yTicks = function (edits) {
	var config = A2(
		$terezka$elm_charts$Internal$Helpers$apply,
		edits,
		{amount: 5, color: '', flip: false, generate: $terezka$elm_charts$Internal$Svg$Floats, grid: true, height: 5, limits: _List_Nil, pinned: $terezka$elm_charts$Chart$Attributes$zero, width: 1});
	var toTicks = function (p) {
		return A2(
			$elm$core$List$map,
			function ($) {
				return $.value;
			},
			A4(
				$terezka$elm_charts$Chart$generateValues,
				config.amount,
				config.generate,
				$elm$core$Maybe$Nothing,
				A3(
					$elm$core$List$foldl,
					F2(
						function (f, y) {
							return f(y);
						}),
					p.y,
					config.limits)));
	};
	var addTickValues = F2(
		function (p, ts) {
			return _Utils_update(
				ts,
				{
					ys: _Utils_ap(
						ts.ys,
						toTicks(p))
				});
		});
	return A2(
		$terezka$elm_charts$Chart$TicksElement,
		addTickValues,
		function (p) {
			var toTick = function (y) {
				return A3(
					$terezka$elm_charts$Chart$Svg$yTick,
					p,
					_List_fromArray(
						[
							$terezka$elm_charts$Chart$Attributes$color(config.color),
							$terezka$elm_charts$Chart$Attributes$length(
							config.flip ? (-config.height) : config.height),
							$terezka$elm_charts$Chart$Attributes$width(config.width)
						]),
					{
						x: config.pinned(p.x),
						y: y
					});
			};
			return A2(
				$elm$svg$Svg$g,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$class('elm-charts__y-ticks')
					]),
				A2(
					$elm$core$List$map,
					toTick,
					toTicks(p)));
		});
};
var $terezka$elm_charts$Chart$Attributes$domain = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{domain: v});
	});
var $terezka$elm_charts$Chart$Attributes$range = F2(
	function (v, config) {
		return _Utils_update(
			config,
			{range: v});
	});
var $author$project$MyCharts$Chart$zoomAttrs = function (zoom) {
	var width = 330;
	var record = $gampleman$elm_visualization$Zoom$asRecord(zoom);
	var scale = record.scale;
	var x = record.translate.x;
	var y = record.translate.y;
	var height = 330;
	var axisFunc = F3(
		function (widthHeight, translation, iAxis) {
			var range = iAxis.dataMax - iAxis.dataMin;
			var trans = (translation * range) / (scale * widthHeight);
			var newRange = range / scale;
			var center = ((iAxis.dataMin + iAxis.dataMax) / 2.0) + (trans / 2);
			var newMax = center + (newRange / 2);
			var newMin = center - (newRange / 2);
			return _Utils_update(
				iAxis,
				{max: newMax, min: newMin});
		});
	return _List_fromArray(
		[
			$terezka$elm_charts$Chart$Attributes$range(
			_List_fromArray(
				[
					A2(axisFunc, width, -x)
				])),
			$terezka$elm_charts$Chart$Attributes$domain(
			_List_fromArray(
				[
					A2(axisFunc, height, y)
				]))
		]);
};
var $author$project$MyCharts$Chart$view = F4(
	function (model, msgToMainMsg, chartData, curves) {
		var zoomModel = function ($) {
			return $.zoomModel;
		}(model);
		var evZoom = function ($) {
			return $.zoom;
		}(model);
		var record = $gampleman$elm_visualization$Zoom$asRecord(evZoom);
		var scale = record.scale;
		var x = record.translate.x;
		var y = record.translate.y;
		return A2(
			$terezka$elm_charts$Chart$chart,
			_Utils_ap(
				_List_fromArray(
					[
						$terezka$elm_charts$Chart$Attributes$height(380),
						$terezka$elm_charts$Chart$Attributes$width(400),
						$terezka$elm_charts$Chart$Attributes$padding(
						{bottom: 0, left: 0, right: 20, top: 10}),
						$terezka$elm_charts$Chart$Attributes$margin(
						{bottom: 20, left: 50, right: 0, top: 40}),
						$terezka$elm_charts$Chart$Attributes$attrs(
						_List_fromArray(
							[
								$gampleman$elm_visualization$Zoom$transform(evZoom)
							])),
						$terezka$elm_charts$Chart$Attributes$attrs(
						A2(
							$gampleman$elm_visualization$Zoom$events,
							evZoom,
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$MyCharts$Chart$EVZoomMsg)))
					]),
				$author$project$MyCharts$Chart$zoomAttrs(evZoom)),
			_Utils_ap(
				_List_fromArray(
					[
						$terezka$elm_charts$Chart$xLabels(
						_List_fromArray(
							[
								$terezka$elm_charts$Chart$Attributes$pinned(
								function ($) {
									return $.min;
								}),
								$terezka$elm_charts$Chart$Attributes$withGrid
							])),
						$terezka$elm_charts$Chart$yLabels(
						_List_fromArray(
							[
								$terezka$elm_charts$Chart$Attributes$pinned(
								function ($) {
									return $.min;
								}),
								$terezka$elm_charts$Chart$Attributes$withGrid
							])),
						$terezka$elm_charts$Chart$xAxis(
						_List_fromArray(
							[
								$terezka$elm_charts$Chart$Attributes$pinned(
								function ($) {
									return $.min;
								})
							])),
						$terezka$elm_charts$Chart$yAxis(_List_Nil),
						$terezka$elm_charts$Chart$xTicks(
						_List_fromArray(
							[
								$terezka$elm_charts$Chart$Attributes$pinned(
								function ($) {
									return $.min;
								})
							])),
						$terezka$elm_charts$Chart$yTicks(_List_Nil),
						A4(
						$terezka$elm_charts$Chart$legendsAt,
						function ($) {
							return $.min;
						},
						function ($) {
							return $.max;
						},
						_List_fromArray(
							[
								$terezka$elm_charts$Chart$Attributes$row,
								$terezka$elm_charts$Chart$Attributes$alignLeft,
								$terezka$elm_charts$Chart$Attributes$moveRight(20),
								$terezka$elm_charts$Chart$Attributes$moveUp(28),
								$terezka$elm_charts$Chart$Attributes$spacing(15)
							]),
						_List_fromArray(
							[
								$terezka$elm_charts$Chart$Attributes$width(20)
							]))
					]),
				function () {
					if (!chartData.b) {
						return _List_Nil;
					} else {
						var chartDatum = chartData.a;
						var cds = chartData.b;
						return A2(
							$elm$core$List$filterMap,
							A2($author$project$MyCharts$Chart$Curve$curveToChartSeries, chartDatum, chartData),
							curves);
					}
				}()));
	});
var $author$project$MyCharts$Chart$AddCurve = {$: 'AddCurve'};
var $author$project$MyCharts$Chart$RemoveCurve = function (a) {
	return {$: 'RemoveCurve', a: a};
};
var $author$project$UI$addCurveButton = function (msg) {
	return A2(
		$mdgriffith$elm_ui$Element$Input$button,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width(
				$mdgriffith$elm_ui$Element$px(36)),
				$mdgriffith$elm_ui$Element$height(
				$mdgriffith$elm_ui$Element$px(36)),
				$mdgriffith$elm_ui$Element$Border$rounded(18),
				$mdgriffith$elm_ui$Element$Border$width(1),
				$mdgriffith$elm_ui$Element$Font$center,
				$mdgriffith$elm_ui$Element$Font$size(16),
				$mdgriffith$elm_ui$Element$Font$color(
				A3($mdgriffith$elm_ui$Element$rgb255, 30, 30, 30))
			]),
		{
			label: A2(
				$mdgriffith$elm_ui$Element$el,
				_List_fromArray(
					[$mdgriffith$elm_ui$Element$centerX, $mdgriffith$elm_ui$Element$Font$center]),
				$mdgriffith$elm_ui$Element$text('+')),
			onPress: $elm$core$Maybe$Just(msg)
		});
};
var $mdgriffith$elm_ui$Internal$Model$Empty = {$: 'Empty'};
var $mdgriffith$elm_ui$Element$none = $mdgriffith$elm_ui$Internal$Model$Empty;
var $author$project$UI$addCurveButtonSpace = A2(
	$mdgriffith$elm_ui$Element$el,
	_List_fromArray(
		[
			$mdgriffith$elm_ui$Element$width(
			$mdgriffith$elm_ui$Element$px(36)),
			$mdgriffith$elm_ui$Element$height(
			$mdgriffith$elm_ui$Element$px(36))
		]),
	$mdgriffith$elm_ui$Element$none);
var $author$project$MyCharts$Chart$CurveMsg = F2(
	function (a, b) {
		return {$: 'CurveMsg', a: a, b: b};
	});
var $author$project$MyCharts$Chart$Curve$XAxis = function (a) {
	return {$: 'XAxis', a: a};
};
var $author$project$MyCharts$Chart$Curve$YAxis = function (a) {
	return {$: 'YAxis', a: a};
};
var $mdgriffith$elm_ui$Element$Font$alignRight = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$fontAlignment, $mdgriffith$elm_ui$Internal$Style$classes.textRight);
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$select = _VirtualDom_node('select');
var $author$project$UI$select = F2(
	function (strToMsg, options) {
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width(
					$mdgriffith$elm_ui$Element$px(80)),
					$mdgriffith$elm_ui$Element$Border$width(0),
					$mdgriffith$elm_ui$Element$Font$alignRight,
					$mdgriffith$elm_ui$Element$padding(10)
				]),
			$mdgriffith$elm_ui$Element$html(
				A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$elm$html$Html$Events$onInput(strToMsg),
							A2($elm$html$Html$Attributes$style, 'border-radius', '0px'),
							A2($elm$html$Html$Attributes$style, 'border-color', 'rgba(100,100,100,0.45)'),
							A2($elm$html$Html$Attributes$style, 'font-family', '\"IBM Plex Sans\", sans-serif'),
							A2($elm$html$Html$Attributes$style, 'border-width', '0 0 1px 0'),
							A2($elm$html$Html$Attributes$style, 'transition', 'all .5s ease'),
							A2($elm$html$Html$Attributes$style, 'font-size', '16px'),
							A2($elm$html$Html$Attributes$style, 'text-align-last', 'center'),
							A2($elm$html$Html$Attributes$style, 'color', '#646464')
						]),
					options)));
	});
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$MyCharts$Chart$Curve$chartAxisOptionView = F3(
	function (val, txt, selStr) {
		return _Utils_eq(selStr, val) ? A2(
			$elm$html$Html$option,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$value(val),
					$elm$html$Html$Attributes$selected(true)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(txt)
				])) : A2(
			$elm$html$Html$option,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$value(val)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(txt)
				]));
	});
var $author$project$MyCharts$Chart$Curve$chartAxisOptionView2 = F3(
	function (chartDatum, optStr, selStr) {
		return A3(
			$author$project$MyCharts$Chart$Curve$chartAxisOptionView,
			optStr,
			A2($author$project$MyCharts$Chart$Curve$optionToNameCurve, chartDatum, optStr),
			selStr);
	});
var $author$project$MyCharts$Chart$Curve$view = F2(
	function (chartData, selStr) {
		if (!chartData.b) {
			return _List_Nil;
		} else {
			var chartDatum = chartData.a;
			var ls = chartData.b;
			switch (chartDatum.$) {
				case 'TS1':
					var datum = chartDatum.a;
					return _List_fromArray(
						[
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 't', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'x1', selStr)
						]);
				case 'TS1E1R1U1':
					var datum = chartDatum.a;
					return _List_fromArray(
						[
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 't', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'x1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'e1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'r1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'u1', selStr)
						]);
				case 'TS1E1R1U4':
					var datum = chartDatum.a;
					return _List_fromArray(
						[
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 't', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'x1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'e1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'r1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'u1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'u2', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'u3', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'u4', selStr)
						]);
				default:
					var datum = chartDatum.a;
					return _List_fromArray(
						[
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 't', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'x1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'x2', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'e1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'r1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'u1', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'u2', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'u3', selStr),
							A3($author$project$MyCharts$Chart$Curve$chartAxisOptionView2, chartDatum, 'u4', selStr)
						]);
			}
		}
	});
var $author$project$MyCharts$Chart$curveSelectionView = F3(
	function (chartData, chartIToMsg, curve) {
		var curveID = function ($) {
			return $.curveID;
		}(curve);
		var _v0 = function ($) {
			return $.axesString;
		}(curve);
		var xstr = _v0.a;
		var ystr = _v0.b;
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[$mdgriffith$elm_ui$Element$centerX]),
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$height(
							$mdgriffith$elm_ui$Element$px(50))
						]),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Element$el,
							_List_Nil,
							$mdgriffith$elm_ui$Element$text(
								'Curve ' + ($elm$core$String$fromInt(curveID) + '   '))),
							A2(
							$author$project$UI$select,
							A2(
								$elm$core$Basics$composeL,
								A2(
									$elm$core$Basics$composeL,
									chartIToMsg,
									$author$project$MyCharts$Chart$CurveMsg(curveID)),
								$author$project$MyCharts$Chart$Curve$XAxis),
							A2($author$project$MyCharts$Chart$Curve$view, chartData, xstr)),
							A2(
							$author$project$UI$select,
							A2(
								$elm$core$Basics$composeL,
								A2(
									$elm$core$Basics$composeL,
									chartIToMsg,
									$author$project$MyCharts$Chart$CurveMsg(curveID)),
								$author$project$MyCharts$Chart$Curve$YAxis),
							A2($author$project$MyCharts$Chart$Curve$view, chartData, ystr))
						]))
				]));
	});
var $author$project$UI$elementNoneWidth = function (_int) {
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width(
				$mdgriffith$elm_ui$Element$px(_int))
			]),
		$mdgriffith$elm_ui$Element$none);
};
var $author$project$UI$removeCurveButton = function (msg) {
	return A2(
		$mdgriffith$elm_ui$Element$Input$button,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width(
				$mdgriffith$elm_ui$Element$px(36)),
				$mdgriffith$elm_ui$Element$height(
				$mdgriffith$elm_ui$Element$px(36)),
				$mdgriffith$elm_ui$Element$Border$rounded(18),
				$mdgriffith$elm_ui$Element$Border$width(1),
				$mdgriffith$elm_ui$Element$Font$center,
				$mdgriffith$elm_ui$Element$Font$size(20),
				$mdgriffith$elm_ui$Element$Font$color(
				A3($mdgriffith$elm_ui$Element$rgb255, 30, 30, 30))
			]),
		{
			label: A2(
				$mdgriffith$elm_ui$Element$el,
				_List_fromArray(
					[$mdgriffith$elm_ui$Element$centerX, $mdgriffith$elm_ui$Element$Font$center]),
				$mdgriffith$elm_ui$Element$text('-')),
			onPress: $elm$core$Maybe$Just(msg)
		});
};
var $author$project$MyCharts$Chart$viewCurves = F3(
	function (chartData, chartIToMsg, curves) {
		var width = 0;
		var spacing = $mdgriffith$elm_ui$Element$spacing(10);
		var align = $mdgriffith$elm_ui$Element$centerX;
		if (!curves.b) {
			return A2(
				$elm$core$List$cons,
				A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[align, spacing]),
					_List_fromArray(
						[
							$author$project$UI$addCurveButton(
							chartIToMsg($author$project$MyCharts$Chart$AddCurve))
						])),
				_List_Nil);
		} else {
			if (!curves.b.b) {
				var c = curves.a;
				return A2(
					$elm$core$List$cons,
					A2(
						$mdgriffith$elm_ui$Element$row,
						_List_fromArray(
							[align, spacing]),
						_List_fromArray(
							[
								A3($author$project$MyCharts$Chart$curveSelectionView, chartData, chartIToMsg, c),
								$author$project$UI$removeCurveButton(
								chartIToMsg(
									$author$project$MyCharts$Chart$RemoveCurve(
										function ($) {
											return $.curveID;
										}(c)))),
								$author$project$UI$addCurveButton(
								chartIToMsg($author$project$MyCharts$Chart$AddCurve)),
								$author$project$UI$elementNoneWidth(width)
							])),
					_List_Nil);
			} else {
				var c = curves.a;
				var cs = curves.b;
				return A2(
					$elm$core$List$cons,
					A2(
						$mdgriffith$elm_ui$Element$row,
						_List_fromArray(
							[align, spacing]),
						_List_fromArray(
							[
								A3($author$project$MyCharts$Chart$curveSelectionView, chartData, chartIToMsg, c),
								$author$project$UI$removeCurveButton(
								chartIToMsg(
									$author$project$MyCharts$Chart$RemoveCurve(
										function ($) {
											return $.curveID;
										}(c)))),
								$author$project$UI$addCurveButtonSpace,
								$author$project$UI$elementNoneWidth(width)
							])),
					A3($author$project$MyCharts$Chart$viewCurves, chartData, chartIToMsg, cs));
			}
		}
	});
var $author$project$MyCharts$view = F3(
	function (chartData, chartModel, msgToMainMsg) {
		var editingCurves = function ($) {
			return $.editingCurves;
		}(chartModel);
		var curves = function ($) {
			return $.curves;
		}(chartModel);
		var chartID = function ($) {
			return $.chartID;
		}(chartModel);
		return A2(
			$author$project$UI$element,
			_List_Nil,
			function () {
				if (editingCurves) {
					return A2(
						$mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$centerX,
								$mdgriffith$elm_ui$Element$spacing(20),
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
							]),
						_Utils_ap(
							_List_fromArray(
								[
									A2(
									$mdgriffith$elm_ui$Element$row,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
										]),
									_List_fromArray(
										[
											A2($author$project$MyCharts$removeChartButton, chartID, msgToMainMsg),
											A2(
											$author$project$MyCharts$buttonToggleChart,
											editingCurves,
											A2(
												$elm$core$Basics$composeL,
												msgToMainMsg,
												$author$project$MyCharts$ChartMsg(chartID)))
										]))
								]),
							A3(
								$author$project$MyCharts$Chart$viewCurves,
								chartData,
								A2(
									$elm$core$Basics$composeL,
									msgToMainMsg,
									$author$project$MyCharts$ChartMsg(chartID)),
								curves)));
				} else {
					return A2(
						$mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$centerX,
								$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill)
							]),
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Element$row,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill)
									]),
								_List_fromArray(
									[
										A2($author$project$MyCharts$removeChartButton, chartID, msgToMainMsg),
										A2(
										$author$project$MyCharts$buttonToggleChart,
										editingCurves,
										A2(
											$elm$core$Basics$composeL,
											msgToMainMsg,
											$author$project$MyCharts$ChartMsg(chartID)))
									])),
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[
										$mdgriffith$elm_ui$Element$centerX,
										$mdgriffith$elm_ui$Element$width(
										$mdgriffith$elm_ui$Element$px(400))
									]),
								$mdgriffith$elm_ui$Element$html(
									A4(
										$author$project$MyCharts$Chart$view,
										chartModel,
										A2(
											$elm$core$Basics$composeL,
											msgToMainMsg,
											$author$project$MyCharts$ChartMsg(chartID)),
										chartData,
										curves)))
							]));
				}
			}());
	});
var $author$project$MyCharts$listMaybeChartToListElement = F3(
	function (listMaybe, chartData, msgToMainMsg) {
		var indexedFunc = F2(
			function (index, maybeChart) {
				if (maybeChart.$ === 'Just') {
					var chartModel = maybeChart.a;
					return A3($author$project$MyCharts$view, chartData, chartModel, msgToMainMsg);
				} else {
					return $author$project$UI$addNewElementSpace(
						msgToMainMsg(
							$author$project$MyCharts$AddChart(index + 1)));
				}
			});
		return A2($elm$core$List$indexedMap, indexedFunc, listMaybe);
	});
var $author$project$MyCharts$listToTuple = function (listElement) {
	if (listElement.b && listElement.b.b) {
		var a = listElement.a;
		var _v1 = listElement.b;
		var b = _v1.a;
		var xs = _v1.b;
		return A2(
			$elm$core$List$cons,
			_Utils_Tuple2(a, b),
			$author$project$MyCharts$listToTuple(xs));
	} else {
		return _List_Nil;
	}
};
var $author$project$MyCharts$maxChartIDFromListCharts = function (model) {
	return A3(
		$elm$core$List$foldr,
		F2(
			function (cp, acc) {
				return A2($elm$core$Basics$max, cp.chartID, acc);
			}),
		0,
		model);
};
var $author$project$MyCharts$chartsTuple = F3(
	function (model, chartData, msgToMainMsg) {
		var maxChartID = $author$project$MyCharts$maxChartIDFromListCharts(model);
		var listMaybe = A2($author$project$MyCharts$fillWithMaybe, model, 1);
		var newListMaybe = (!A2($elm$core$Basics$modBy, 2, maxChartID)) ? _Utils_ap(
			listMaybe,
			_List_fromArray(
				[$elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing])) : _Utils_ap(
			listMaybe,
			_List_fromArray(
				[$elm$core$Maybe$Nothing]));
		var listElement = A3($author$project$MyCharts$listMaybeChartToListElement, newListMaybe, chartData, msgToMainMsg);
		return $author$project$MyCharts$listToTuple(listElement);
	});
var $author$project$EdoSolver$getRsUs = F5(
	function (xs, edoParam, outFunc, refFunc, controller) {
		var tempo = function ($) {
			return $.tempo;
		}(edoParam);
		var ys = A2(outFunc, tempo, xs);
		var rs = A2(refFunc, tempo, ys);
		var passo = function ($) {
			return $.passo;
		}(edoParam);
		var es = A3($author$project$EdoSolver$zipWith, $elm$core$Basics$sub, rs, ys);
		var controlMemory = function ($) {
			return $.controlMemory;
		}(edoParam);
		var _v0 = A5(controller, controlMemory, es, passo, tempo, xs);
		var us = _v0.a;
		var controlMemory2 = _v0.b;
		return _Utils_Tuple2(rs, us);
	});
var $mdgriffith$elm_ui$Internal$Model$Max = F2(
	function (a, b) {
		return {$: 'Max', a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$maximum = F2(
	function (i, l) {
		return A2($mdgriffith$elm_ui$Internal$Model$Max, i, l);
	});
var $mdgriffith$elm_ui$Internal$Model$Min = F2(
	function (a, b) {
		return {$: 'Min', a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$minimum = F2(
	function (i, l) {
		return A2($mdgriffith$elm_ui$Internal$Model$Min, i, l);
	});
var $mdgriffith$elm_ui$Element$moveDown = function (y) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$TransformComponent,
		$mdgriffith$elm_ui$Internal$Flag$moveY,
		$mdgriffith$elm_ui$Internal$Model$MoveY(y));
};
var $author$project$ModelSystem$output = function (modelType) {
	if (modelType.$ === 'Level') {
		return $author$project$ModelSystem$Level$output;
	} else {
		return $author$project$ModelSystem$Level2$output;
	}
};
var $author$project$DataConvert$rsFromDatum = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return _List_Nil;
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return _List_fromArray(
				[
					function ($) {
					return $.r1;
				}(datum)
				]);
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return _List_fromArray(
				[
					function ($) {
					return $.r1;
				}(datum)
				]);
		default:
			var datum = chartDatum.a;
			return _List_fromArray(
				[
					function ($) {
					return $.r1;
				}(datum)
				]);
	}
};
var $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox2d = function (a) {
	return {$: 'BoundingBox2d', a: a};
};
var $ianmackenzie$elm_units$Quantity$Quantity = function (a) {
	return {$: 'Quantity', a: a};
};
var $ianmackenzie$elm_geometry$Geometry$Types$Frame2d = function (a) {
	return {$: 'Frame2d', a: a};
};
var $ianmackenzie$elm_geometry$Frame2d$copy = function (_v0) {
	var properties = _v0.a;
	return $ianmackenzie$elm_geometry$Geometry$Types$Frame2d(properties);
};
var $ianmackenzie$elm_geometry$Rectangle2d$axes = function (_v0) {
	var rectangle = _v0.a;
	return $ianmackenzie$elm_geometry$Frame2d$copy(rectangle.axes);
};
var $ianmackenzie$elm_geometry$Rectangle2d$dimensions = function (_v0) {
	var rectangle = _v0.a;
	return rectangle.dimensions;
};
var $ianmackenzie$elm_geometry$Frame2d$originPoint = function (_v0) {
	var frame = _v0.a;
	return frame.originPoint;
};
var $ianmackenzie$elm_geometry$Direction2d$unwrap = function (_v0) {
	var directionComponents = _v0.a;
	return directionComponents;
};
var $ianmackenzie$elm_geometry$Point2d$unwrap = function (_v0) {
	var pointCoordinates = _v0.a;
	return pointCoordinates;
};
var $ianmackenzie$elm_geometry$Frame2d$xDirection = function (_v0) {
	var frame = _v0.a;
	return frame.xDirection;
};
var $ianmackenzie$elm_geometry$Frame2d$yDirection = function (_v0) {
	var frame = _v0.a;
	return frame.yDirection;
};
var $ianmackenzie$elm_geometry$Rectangle2d$boundingBox = function (rectangle) {
	var frame = $ianmackenzie$elm_geometry$Rectangle2d$axes(rectangle);
	var i = $ianmackenzie$elm_geometry$Direction2d$unwrap(
		$ianmackenzie$elm_geometry$Frame2d$xDirection(frame));
	var ix = $elm$core$Basics$abs(i.x);
	var iy = $elm$core$Basics$abs(i.y);
	var j = $ianmackenzie$elm_geometry$Direction2d$unwrap(
		$ianmackenzie$elm_geometry$Frame2d$yDirection(frame));
	var jx = $elm$core$Basics$abs(j.x);
	var jy = $elm$core$Basics$abs(j.y);
	var p0 = $ianmackenzie$elm_geometry$Point2d$unwrap(
		$ianmackenzie$elm_geometry$Frame2d$originPoint(frame));
	var _v0 = $ianmackenzie$elm_geometry$Rectangle2d$dimensions(rectangle);
	var width = _v0.a.a;
	var height = _v0.b.a;
	var halfHeight = height / 2;
	var halfWidth = width / 2;
	var maxX = (p0.x + (ix * halfWidth)) + (jx * halfHeight);
	var maxY = (p0.y + (iy * halfWidth)) + (jy * halfHeight);
	var minX = (p0.x - (ix * halfWidth)) - (jx * halfHeight);
	var minY = (p0.y - (iy * halfWidth)) - (jy * halfHeight);
	return $ianmackenzie$elm_geometry$Geometry$Types$BoundingBox2d(
		{
			maxX: $ianmackenzie$elm_units$Quantity$Quantity(maxX),
			maxY: $ianmackenzie$elm_units$Quantity$Quantity(maxY),
			minX: $ianmackenzie$elm_units$Quantity$Quantity(minX),
			minY: $ianmackenzie$elm_units$Quantity$Quantity(minY)
		});
};
var $ianmackenzie$elm_geometry$Quantity$Extra$lOverTheta = F2(
	function (_v0, _v1) {
		var l = _v0.a;
		var theta = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(l / theta);
	});
var $ianmackenzie$elm_units$Quantity$minus = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(x - y);
	});
var $ianmackenzie$elm_units$Quantity$multiplyBy = F2(
	function (scale, _v0) {
		var value = _v0.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(scale * value);
	});
var $ianmackenzie$elm_units$Quantity$plus = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(x + y);
	});
var $ianmackenzie$elm_geometry$Direction2d$xComponent = function (_v0) {
	var d = _v0.a;
	return d.x;
};
var $ianmackenzie$elm_geometry$Point2d$xCoordinate = function (_v0) {
	var p = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(p.x);
};
var $ianmackenzie$elm_geometry$Geometry$Types$Point2d = function (a) {
	return {$: 'Point2d', a: a};
};
var $ianmackenzie$elm_geometry$Point2d$xy = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v1.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point2d(
			{x: x, y: y});
	});
var $ianmackenzie$elm_geometry$Direction2d$yComponent = function (_v0) {
	var d = _v0.a;
	return d.y;
};
var $ianmackenzie$elm_geometry$Point2d$yCoordinate = function (_v0) {
	var p = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(p.y);
};
var $ianmackenzie$elm_geometry$Arc2d$centerPoint = function (_v0) {
	var arc = _v0.a;
	var y0 = $ianmackenzie$elm_geometry$Point2d$yCoordinate(arc.startPoint);
	var x0 = $ianmackenzie$elm_geometry$Point2d$xCoordinate(arc.startPoint);
	var r = A2($ianmackenzie$elm_geometry$Quantity$Extra$lOverTheta, arc.signedLength, arc.sweptAngle);
	var dy = $ianmackenzie$elm_geometry$Direction2d$yComponent(arc.xDirection);
	var dx = $ianmackenzie$elm_geometry$Direction2d$xComponent(arc.xDirection);
	var cy = A2(
		$ianmackenzie$elm_units$Quantity$plus,
		A2($ianmackenzie$elm_units$Quantity$multiplyBy, dx, r),
		y0);
	var cx = A2(
		$ianmackenzie$elm_units$Quantity$minus,
		A2($ianmackenzie$elm_units$Quantity$multiplyBy, dy, r),
		x0);
	return A2($ianmackenzie$elm_geometry$Point2d$xy, cx, cy);
};
var $ianmackenzie$elm_units$Angle$radians = function (numRadians) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numRadians);
};
var $ianmackenzie$elm_units$Angle$degrees = function (numDegrees) {
	return $ianmackenzie$elm_units$Angle$radians($elm$core$Basics$pi * (numDegrees / 180));
};
var $author$project$Drawing2d$Attributes$DominantBaseline = function (a) {
	return {$: 'DominantBaseline', a: a};
};
var $author$project$Drawing2d$alphabeticBaseline = $author$project$Drawing2d$Attributes$DominantBaseline('alphabetic');
var $author$project$Drawing2d$Attributes$TextAnchor = function (a) {
	return {$: 'TextAnchor', a: a};
};
var $author$project$Drawing2d$anchorAtStart = $author$project$Drawing2d$Attributes$TextAnchor('start');
var $ianmackenzie$elm_units$Quantity$at = F2(
	function (_v0, _v1) {
		var rateOfChange = _v0.a;
		var independentValue = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(rateOfChange * independentValue);
	});
var $ianmackenzie$elm_units$Quantity$at_ = F2(
	function (_v0, _v1) {
		var rateOfChange = _v0.a;
		var dependentValue = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(dependentValue / rateOfChange);
	});
var $author$project$Drawing2d$Attributes$BevelJoin = {$: 'BevelJoin'};
var $author$project$Drawing2d$Attributes$StrokeLineJoin = function (a) {
	return {$: 'StrokeLineJoin', a: a};
};
var $author$project$Drawing2d$bevelStrokeJoins = $author$project$Drawing2d$Attributes$StrokeLineJoin($author$project$Drawing2d$Attributes$BevelJoin);
var $author$project$Drawing2d$Attributes$StrokeColor = function (a) {
	return {$: 'StrokeColor', a: a};
};
var $author$project$Drawing2d$Attributes$StrokeStyle = function (a) {
	return {$: 'StrokeStyle', a: a};
};
var $author$project$Drawing2d$blackStroke = $author$project$Drawing2d$Attributes$StrokeStyle(
	$author$project$Drawing2d$Attributes$StrokeColor('black'));
var $author$project$Drawing2d$Attributes$TextColor = function (a) {
	return {$: 'TextColor', a: a};
};
var $author$project$Drawing2d$blackText = $author$project$Drawing2d$Attributes$TextColor('black');
var $author$project$Drawing2d$containerStaticCss = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'relative'),
		A2($elm$html$Html$Attributes$style, 'padding-top', '0px'),
		A2($elm$html$Html$Attributes$style, 'padding-right', '0px'),
		A2($elm$html$Html$Attributes$style, 'padding-left', '0px'),
		A2($elm$html$Html$Attributes$style, 'overflow', 'hidden')
	]);
var $author$project$Drawing2d$Attributes$FontSize = function (a) {
	return {$: 'FontSize', a: a};
};
var $author$project$Drawing2d$fontSize = function (givenSize) {
	return $author$project$Drawing2d$Attributes$FontSize(givenSize);
};
var $author$project$Drawing2d$Entity = function (a) {
	return {$: 'Entity', a: a};
};
var $elm$svg$Svg$Attributes$dx = _VirtualDom_attribute('dx');
var $elm$svg$Svg$Attributes$dy = _VirtualDom_attribute('dy');
var $elm$svg$Svg$filter = $elm$svg$Svg$trustedNode('filter');
var $elm$svg$Svg$Attributes$floodColor = _VirtualDom_attribute('flood-color');
var $elm$svg$Svg$Attributes$floodOpacity = _VirtualDom_attribute('flood-opacity');
var $elm$virtual_dom$VirtualDom$nodeNS = function (tag) {
	return _VirtualDom_nodeNS(
		_VirtualDom_noScript(tag));
};
var $elm$svg$Svg$Attributes$stdDeviation = _VirtualDom_attribute('stdDeviation');
var $author$project$Drawing2d$Shadow$element = function (_v0) {
	var shadow = _v0.a;
	return A2(
		$elm$svg$Svg$filter,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$id(shadow.id)
			]),
		_List_fromArray(
			[
				A4(
				$elm$virtual_dom$VirtualDom$nodeNS,
				'http://www.w3.org/2000/svg',
				'feDropShadow',
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$dx(shadow.dx),
						$elm$svg$Svg$Attributes$dy(shadow.dy),
						$elm$svg$Svg$Attributes$stdDeviation(shadow.stdDeviation),
						$elm$svg$Svg$Attributes$floodColor(shadow.floodColor),
						$elm$svg$Svg$Attributes$floodOpacity(shadow.floodOpacity)
					]),
				_List_Nil)
			]));
};
var $author$project$Drawing2d$addDropShadow = F2(
	function (attributeValues, svgElements) {
		var _v0 = attributeValues.dropShadow;
		if (_v0.$ === 'Nothing') {
			return svgElements;
		} else {
			var shadow = _v0.a;
			return A2(
				$elm$core$List$cons,
				$author$project$Drawing2d$Shadow$element(shadow),
				svgElements);
		}
	});
var $ianmackenzie$elm_geometry$Circle2d$centerPoint = function (_v0) {
	var properties = _v0.a;
	return properties.centerPoint;
};
var $elm$svg$Svg$Attributes$fx = _VirtualDom_attribute('fx');
var $elm$svg$Svg$Attributes$fy = _VirtualDom_attribute('fy');
var $elm$svg$Svg$Attributes$gradientUnits = _VirtualDom_attribute('gradientUnits');
var $author$project$Drawing2d$Stops$id = function (stops) {
	if (stops.$ === 'Values') {
		var stopsId = stops.a;
		var values = stops.b;
		return stopsId;
	} else {
		var stopsId = stops.a;
		return stopsId;
	}
};
var $elm$svg$Svg$radialGradient = $elm$svg$Svg$trustedNode('radialGradient');
var $ianmackenzie$elm_geometry$Circle2d$radius = function (_v0) {
	var properties = _v0.a;
	return properties.radius;
};
var $author$project$Drawing2d$Stops$stopElement = function (_v0) {
	var offset = _v0.offset;
	var color = _v0.color;
	return A2(
		$elm$svg$Svg$stop,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$offset(offset),
				$elm$svg$Svg$Attributes$stopColor(color)
			]),
		_List_Nil);
};
var $author$project$Drawing2d$Stops$render = F3(
	function (toSvgElement, stops, accumulated) {
		if (stops.$ === 'Values') {
			var stopsId = stops.a;
			var stopValues = stops.b;
			var svgElement = A2(
				toSvgElement,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$id(stopsId)
					]),
				A2($elm$core$List$map, $author$project$Drawing2d$Stops$stopElement, stopValues));
			return A2($elm$core$List$cons, svgElement, accumulated);
		} else {
			return accumulated;
		}
	});
var $elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var $author$project$Drawing2d$Gradient$render = F2(
	function (gradient, svgElements) {
		if (gradient.$ === 'LinearGradient') {
			var linearGradient = gradient.a;
			var stopsId = $author$project$Drawing2d$Stops$id(linearGradient.stops);
			var p2 = $ianmackenzie$elm_geometry$Point2d$unwrap(linearGradient.end);
			var p1 = $ianmackenzie$elm_geometry$Point2d$unwrap(linearGradient.start);
			var gradientElement = A2(
				$elm$svg$Svg$linearGradient,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$id(linearGradient.id),
						$elm$svg$Svg$Attributes$x1(
						$elm$core$String$fromFloat(p1.x)),
						$elm$svg$Svg$Attributes$y1(
						$elm$core$String$fromFloat(-p1.y)),
						$elm$svg$Svg$Attributes$x2(
						$elm$core$String$fromFloat(p2.x)),
						$elm$svg$Svg$Attributes$y2(
						$elm$core$String$fromFloat(-p2.y)),
						$elm$svg$Svg$Attributes$gradientUnits('userSpaceOnUse'),
						$elm$svg$Svg$Attributes$xlinkHref('#' + stopsId)
					]),
				_List_Nil);
			return A3(
				$author$project$Drawing2d$Stops$render,
				$elm$svg$Svg$linearGradient,
				linearGradient.stops,
				A2($elm$core$List$cons, gradientElement, svgElements));
		} else {
			var radialGradient = gradient.a;
			var stopsId = $author$project$Drawing2d$Stops$id(radialGradient.stops);
			var f = $ianmackenzie$elm_geometry$Point2d$unwrap(radialGradient.start);
			var c = $ianmackenzie$elm_geometry$Point2d$unwrap(
				$ianmackenzie$elm_geometry$Circle2d$centerPoint(radialGradient.end));
			var _v1 = $ianmackenzie$elm_geometry$Circle2d$radius(radialGradient.end);
			var r = _v1.a;
			var gradientElement = A2(
				$elm$svg$Svg$radialGradient,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$id(radialGradient.id),
						$elm$svg$Svg$Attributes$fx(
						$elm$core$String$fromFloat(f.x)),
						$elm$svg$Svg$Attributes$fy(
						$elm$core$String$fromFloat(-f.y)),
						$elm$svg$Svg$Attributes$cx(
						$elm$core$String$fromFloat(c.x)),
						$elm$svg$Svg$Attributes$cy(
						$elm$core$String$fromFloat(-c.y)),
						$elm$svg$Svg$Attributes$r(
						$elm$core$String$fromFloat(r)),
						$elm$svg$Svg$Attributes$gradientUnits('userSpaceOnUse'),
						$elm$svg$Svg$Attributes$xlinkHref('#' + stopsId)
					]),
				_List_Nil);
			return A3(
				$author$project$Drawing2d$Stops$render,
				$elm$svg$Svg$radialGradient,
				radialGradient.stops,
				A2($elm$core$List$cons, gradientElement, svgElements));
		}
	});
var $author$project$Drawing2d$addFillGradient = F2(
	function (attributeValues, svgElements) {
		var _v0 = attributeValues.fillStyle;
		if (_v0.$ === 'Nothing') {
			return svgElements;
		} else {
			switch (_v0.a.$) {
				case 'NoFill':
					var _v1 = _v0.a;
					return svgElements;
				case 'TransparentFill':
					var _v2 = _v0.a;
					return svgElements;
				case 'FillColor':
					return svgElements;
				default:
					var gradient = _v0.a.a;
					return A2($author$project$Drawing2d$Gradient$render, gradient, svgElements);
			}
		}
	});
var $author$project$Drawing2d$addStrokeGradient = F2(
	function (attributeValues, svgElements) {
		var _v0 = attributeValues.strokeStyle;
		if (_v0.$ === 'Nothing') {
			return svgElements;
		} else {
			if (_v0.a.$ === 'StrokeColor') {
				return svgElements;
			} else {
				var gradient = _v0.a.a;
				return A2($author$project$Drawing2d$Gradient$render, gradient, svgElements);
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $author$project$Drawing2d$Attributes$registerEventHandler = F2(
	function (_v0, attributeValues) {
		var eventName = _v0.a;
		var handler = _v0.b;
		return _Utils_update(
			attributeValues,
			{
				eventHandlers: A3(
					$elm$core$Dict$update,
					eventName,
					function (registeredHandlers) {
						if (registeredHandlers.$ === 'Nothing') {
							return $elm$core$Maybe$Just(
								_List_fromArray(
									[handler]));
						} else {
							var existingHandlers = registeredHandlers.a;
							return $elm$core$Maybe$Just(
								A2($elm$core$List$cons, handler, existingHandlers));
						}
					},
					attributeValues.eventHandlers)
			});
	});
var $author$project$Drawing2d$Attributes$setAttribute = F2(
	function (attribute, attributeValues) {
		switch (attribute.$) {
			case 'FillStyle':
				var fill = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						fillStyle: $elm$core$Maybe$Just(fill)
					});
			case 'StrokeStyle':
				var stroke = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						strokeStyle: $elm$core$Maybe$Just(stroke)
					});
			case 'Opacity':
				var opacity = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						opacity: $elm$core$Maybe$Just(opacity)
					});
			case 'FontSize':
				var size = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						fontSize: $elm$core$Maybe$Just(size)
					});
			case 'StrokeWidth':
				var width = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						strokeWidth: $elm$core$Maybe$Just(width)
					});
			case 'StrokeLineJoin':
				var lineJoin = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						strokeLineJoin: $elm$core$Maybe$Just(lineJoin)
					});
			case 'StrokeLineCap':
				var lineCap = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						strokeLineCap: $elm$core$Maybe$Just(lineCap)
					});
			case 'StrokeDashPattern':
				var dashPattern = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						strokeDashPattern: $elm$core$Maybe$Just(dashPattern)
					});
			case 'BorderVisibility':
				var bordersVisible = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						borderVisibility: $elm$core$Maybe$Just(bordersVisible)
					});
			case 'DropShadow':
				var shadow = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						dropShadow: $elm$core$Maybe$Just(shadow)
					});
			case 'TextColor':
				var string = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						textColor: $elm$core$Maybe$Just(string)
					});
			case 'FontFamily':
				var string = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						fontFamily: $elm$core$Maybe$Just(string)
					});
			case 'TextAnchor':
				var string = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						textAnchor: $elm$core$Maybe$Just(string)
					});
			case 'DominantBaseline':
				var string = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						dominantBaseline: $elm$core$Maybe$Just(string)
					});
			case 'Cursor':
				var cursor = attribute.a;
				return _Utils_update(
					attributeValues,
					{
						cursor: $elm$core$Maybe$Just(cursor)
					});
			default:
				var eventHandlers = attribute.a;
				return A3($elm$core$List$foldl, $author$project$Drawing2d$Attributes$registerEventHandler, attributeValues, eventHandlers);
		}
	});
var $author$project$Drawing2d$Attributes$assignAttributes = F2(
	function (attributeList, attributeValues) {
		return A3($elm$core$List$foldr, $author$project$Drawing2d$Attributes$setAttribute, attributeValues, attributeList);
	});
var $author$project$Drawing2d$Attributes$emptyAttributeValues = {borderVisibility: $elm$core$Maybe$Nothing, cursor: $elm$core$Maybe$Nothing, dominantBaseline: $elm$core$Maybe$Nothing, dropShadow: $elm$core$Maybe$Nothing, eventHandlers: $elm$core$Dict$empty, fillStyle: $elm$core$Maybe$Nothing, fontFamily: $elm$core$Maybe$Nothing, fontSize: $elm$core$Maybe$Nothing, opacity: $elm$core$Maybe$Nothing, strokeDashPattern: $elm$core$Maybe$Nothing, strokeLineCap: $elm$core$Maybe$Nothing, strokeLineJoin: $elm$core$Maybe$Nothing, strokeStyle: $elm$core$Maybe$Nothing, strokeWidth: $elm$core$Maybe$Nothing, textAnchor: $elm$core$Maybe$Nothing, textColor: $elm$core$Maybe$Nothing};
var $author$project$Drawing2d$Attributes$collectAttributeValues = function (attributeList) {
	return A2($author$project$Drawing2d$Attributes$assignAttributes, attributeList, $author$project$Drawing2d$Attributes$emptyAttributeValues);
};
var $author$project$Drawing2d$Attributes$noFill = $elm$svg$Svg$Attributes$fill('none');
var $author$project$Drawing2d$Gradient$reference = function (gradient) {
	var gradientId = function () {
		if (gradient.$ === 'LinearGradient') {
			var id = gradient.a.id;
			return id;
		} else {
			var id = gradient.a.id;
			return id;
		}
	}();
	return 'url(#' + (gradientId + ')');
};
var $author$project$Drawing2d$Attributes$addFillStyle = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.fillStyle;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			switch (_v0.a.$) {
				case 'NoFill':
					var _v1 = _v0.a;
					return A2($elm$core$List$cons, $author$project$Drawing2d$Attributes$noFill, svgAttributes);
				case 'TransparentFill':
					var _v2 = _v0.a;
					return A2(
						$elm$core$List$cons,
						$elm$svg$Svg$Attributes$fill('black'),
						A2(
							$elm$core$List$cons,
							$elm$svg$Svg$Attributes$fillOpacity('0'),
							svgAttributes));
				case 'FillColor':
					var string = _v0.a.a;
					return A2(
						$elm$core$List$cons,
						$elm$svg$Svg$Attributes$fill(string),
						svgAttributes);
				default:
					var gradient = _v0.a.a;
					return A2(
						$elm$core$List$cons,
						$elm$svg$Svg$Attributes$fill(
							$author$project$Drawing2d$Gradient$reference(gradient)),
						svgAttributes);
			}
		}
	});
var $elm$svg$Svg$Attributes$cursor = _VirtualDom_attribute('cursor');
var $author$project$Drawing2d$Attributes$cursorString = function (cursor) {
	switch (cursor.$) {
		case 'AutoCursor':
			return 'auto';
		case 'DefaultCursor':
			return 'default';
		case 'NoCursor':
			return 'none';
		case 'ContextMenuCursor':
			return 'context-menu';
		case 'HelpCursor':
			return 'help';
		case 'PointerCursor':
			return 'pointer';
		case 'ProgressCursor':
			return 'progress';
		case 'WaitCursor':
			return 'wait';
		case 'CellCursor':
			return 'cell';
		case 'CrosshairCursor':
			return 'crosshair';
		case 'TextCursor':
			return 'text';
		case 'VerticalTextCursor':
			return 'vertical-text';
		case 'AliasCursor':
			return 'alias';
		case 'CopyCursor':
			return 'copy';
		case 'MoveCursor':
			return 'move';
		case 'NoDropCursor':
			return 'no-drop';
		case 'NotAllowedCursor':
			return 'not-allowed';
		case 'GrabCursor':
			return 'grab';
		case 'GrabbingCursor':
			return 'grabbing';
		case 'AllScrollCursor':
			return 'all-scroll';
		case 'ColResizeCursor':
			return 'col-resize';
		case 'RowResizeCursor':
			return 'row-resize';
		case 'NResizeCursor':
			return 'n-resize';
		case 'EResizeCursor':
			return 'e-resize';
		case 'SResizeCursor':
			return 's-resize';
		case 'WResizeCursor':
			return 'w-resize';
		case 'NeResizeCursor':
			return 'ne-resize';
		case 'NwResizeCursor':
			return 'nw-resize';
		case 'SeResizeCursor':
			return 'se-resize';
		case 'SwResizeCursor':
			return 'sw-resize';
		case 'EwResizeCursor':
			return 'ew-resize';
		case 'NsResizeCursor':
			return 'ns-resize';
		case 'NeswResizeCursor':
			return 'nesw-resize';
		case 'NwseResizeCursor':
			return 'nwse-resize';
		case 'ZoomInCursor':
			return 'zoom-in';
		case 'ZoomOutCursor':
			return 'zoom-out';
		default:
			var url = cursor.a;
			var x = cursor.b;
			var y = cursor.c;
			var fallback = cursor.d;
			return 'url' + ('(' + (url + (') ' + ($elm$core$String$fromFloat(x) + (' ' + ($elm$core$String$fromFloat(y) + (', ' + $author$project$Drawing2d$Attributes$cursorString(fallback))))))));
	}
};
var $author$project$Drawing2d$Attributes$addCursor = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.cursor;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var cursor = _v0.a;
			var cursorAttribute = $elm$svg$Svg$Attributes$cursor(
				$author$project$Drawing2d$Attributes$cursorString(cursor));
			return A2($elm$core$List$cons, cursorAttribute, svgAttributes);
		}
	});
var $author$project$Drawing2d$Attributes$preventDefaultAndStopPropagation = $elm$json$Json$Decode$map(
	function (message) {
		return {message: message, preventDefault: true, stopPropagation: true};
	});
var $author$project$Drawing2d$Attributes$on = F2(
	function (eventName, decoder) {
		return A2(
			$elm$svg$Svg$Events$custom,
			eventName,
			$author$project$Drawing2d$Attributes$preventDefaultAndStopPropagation(decoder));
	});
var $author$project$Drawing2d$Attributes$addEventHandler = F3(
	function (eventName, decoders, svgAttributes) {
		return A2(
			$elm$core$List$cons,
			A2(
				$author$project$Drawing2d$Attributes$on,
				eventName,
				$elm$json$Json$Decode$oneOf(decoders)),
			svgAttributes);
	});
var $author$project$Drawing2d$Attributes$suppressTouchActions = F2(
	function (attributeValues, svgAttributes) {
		return (A2($elm$core$Dict$member, 'touchstart', attributeValues.eventHandlers) || (A2($elm$core$Dict$member, 'touchmove', attributeValues.eventHandlers) || A2($elm$core$Dict$member, 'touchend', attributeValues.eventHandlers))) ? A2(
			$elm$core$List$cons,
			A2($elm$html$Html$Attributes$style, 'touch-action', 'none'),
			svgAttributes) : svgAttributes;
	});
var $author$project$Drawing2d$Attributes$addEventHandlers = F2(
	function (attributeValues, svgAttributes) {
		return A2(
			$author$project$Drawing2d$Attributes$suppressTouchActions,
			attributeValues,
			A3($elm$core$Dict$foldl, $author$project$Drawing2d$Attributes$addEventHandler, svgAttributes, attributeValues.eventHandlers));
	});
var $elm$svg$Svg$Attributes$opacity = _VirtualDom_attribute('opacity');
var $author$project$Drawing2d$Attributes$addOpacity = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.opacity;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var opacity = _v0.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$opacity(
					$elm$core$String$fromFloat(opacity)),
				svgAttributes);
		}
	});
var $elm$svg$Svg$Attributes$filter = _VirtualDom_attribute('filter');
var $author$project$Drawing2d$Shadow$reference = function (_v0) {
	var shadow = _v0.a;
	return 'url(#' + (shadow.id + ')');
};
var $author$project$Drawing2d$Attributes$addShadowFilter = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.dropShadow;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var shadow = _v0.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$filter(
					$author$project$Drawing2d$Shadow$reference(shadow)),
				svgAttributes);
		}
	});
var $author$project$Drawing2d$Attributes$addGenericAttributes = F2(
	function (attributeValues, svgAttributes) {
		return A2(
			$author$project$Drawing2d$Attributes$addEventHandlers,
			attributeValues,
			A2(
				$author$project$Drawing2d$Attributes$addOpacity,
				attributeValues,
				A2(
					$author$project$Drawing2d$Attributes$addCursor,
					attributeValues,
					A2($author$project$Drawing2d$Attributes$addShadowFilter, attributeValues, svgAttributes))));
	});
var $ianmackenzie$elm_units$Quantity$unwrap = function (_v0) {
	var value = _v0.a;
	return value;
};
var $author$project$Drawing2d$Attributes$dashPatternSvgAttribute = function (dashPattern) {
	if (!dashPattern.b) {
		return $elm$svg$Svg$Attributes$strokeDasharray('none');
	} else {
		return $elm$svg$Svg$Attributes$strokeDasharray(
			A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$map,
					A2($elm$core$Basics$composeR, $ianmackenzie$elm_units$Quantity$unwrap, $elm$core$String$fromFloat),
					dashPattern)));
	}
};
var $author$project$Drawing2d$Attributes$addStrokeDashPattern = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.strokeDashPattern;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var dashPattern = _v0.a;
			return A2(
				$elm$core$List$cons,
				$author$project$Drawing2d$Attributes$dashPatternSvgAttribute(dashPattern),
				svgAttributes);
		}
	});
var $author$project$Drawing2d$Attributes$lineCapString = function (lineJoin) {
	switch (lineJoin.$) {
		case 'NoCap':
			return 'butt';
		case 'SquareCap':
			return 'square';
		default:
			return 'round';
	}
};
var $elm$svg$Svg$Attributes$strokeLinecap = _VirtualDom_attribute('stroke-linecap');
var $author$project$Drawing2d$Attributes$addStrokeLineCap = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.strokeLineCap;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var lineCap = _v0.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$strokeLinecap(
					$author$project$Drawing2d$Attributes$lineCapString(lineCap)),
				svgAttributes);
		}
	});
var $author$project$Drawing2d$Attributes$lineJoinString = function (lineJoin) {
	switch (lineJoin.$) {
		case 'BevelJoin':
			return 'bevel';
		case 'RoundJoin':
			return 'round';
		default:
			return 'miter';
	}
};
var $elm$svg$Svg$Attributes$strokeLinejoin = _VirtualDom_attribute('stroke-linejoin');
var $author$project$Drawing2d$Attributes$addStrokeLineJoin = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.strokeLineJoin;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var lineJoin = _v0.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$strokeLinejoin(
					$author$project$Drawing2d$Attributes$lineJoinString(lineJoin)),
				svgAttributes);
		}
	});
var $author$project$Drawing2d$Attributes$addStrokeStyle = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.strokeStyle;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			if (_v0.a.$ === 'StrokeColor') {
				var string = _v0.a.a;
				return A2(
					$elm$core$List$cons,
					$elm$svg$Svg$Attributes$stroke(string),
					svgAttributes);
			} else {
				var gradient = _v0.a.a;
				return A2(
					$elm$core$List$cons,
					$elm$svg$Svg$Attributes$stroke(
						$author$project$Drawing2d$Gradient$reference(gradient)),
					svgAttributes);
			}
		}
	});
var $author$project$Drawing2d$Attributes$addStrokeWidth = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.strokeWidth;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var width = _v0.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$strokeWidth(
					$elm$core$String$fromFloat(
						$ianmackenzie$elm_units$Quantity$unwrap(width))),
				svgAttributes);
		}
	});
var $author$project$Drawing2d$Attributes$addStrokeAttributes = F2(
	function (attributeValues, svgAttributes) {
		return A2(
			$author$project$Drawing2d$Attributes$addStrokeDashPattern,
			attributeValues,
			A2(
				$author$project$Drawing2d$Attributes$addStrokeLineCap,
				attributeValues,
				A2(
					$author$project$Drawing2d$Attributes$addStrokeLineJoin,
					attributeValues,
					A2(
						$author$project$Drawing2d$Attributes$addStrokeWidth,
						attributeValues,
						A2($author$project$Drawing2d$Attributes$addStrokeStyle, attributeValues, svgAttributes)))));
	});
var $elm$svg$Svg$Attributes$dominantBaseline = _VirtualDom_attribute('dominant-baseline');
var $author$project$Drawing2d$Attributes$addDominantBaseline = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.dominantBaseline;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var string = _v0.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$dominantBaseline(string),
				svgAttributes);
		}
	});
var $elm$svg$Svg$Attributes$fontFamily = _VirtualDom_attribute('font-family');
var $author$project$Drawing2d$Attributes$addFontFamily = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.fontFamily;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var string = _v0.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$fontFamily(string),
				svgAttributes);
		}
	});
var $elm$svg$Svg$Attributes$fontSize = _VirtualDom_attribute('font-size');
var $author$project$Drawing2d$Attributes$addFontSize = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.fontSize;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var size = _v0.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$fontSize(
					$elm$core$String$fromFloat(
						$ianmackenzie$elm_units$Quantity$unwrap(size))),
				svgAttributes);
		}
	});
var $elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var $author$project$Drawing2d$Attributes$addTextAnchor = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.textAnchor;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var string = _v0.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$textAnchor(string),
				svgAttributes);
		}
	});
var $elm$svg$Svg$Attributes$color = _VirtualDom_attribute('color');
var $author$project$Drawing2d$Attributes$addTextColor = F2(
	function (attributeValues, svgAttributes) {
		var _v0 = attributeValues.textColor;
		if (_v0.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var string = _v0.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$color(string),
				svgAttributes);
		}
	});
var $author$project$Drawing2d$Attributes$addTextAttributes = F2(
	function (attributeValues, svgAttributes) {
		return A2(
			$author$project$Drawing2d$Attributes$addTextColor,
			attributeValues,
			A2(
				$author$project$Drawing2d$Attributes$addDominantBaseline,
				attributeValues,
				A2(
					$author$project$Drawing2d$Attributes$addTextAnchor,
					attributeValues,
					A2(
						$author$project$Drawing2d$Attributes$addFontSize,
						attributeValues,
						A2($author$project$Drawing2d$Attributes$addFontFamily, attributeValues, svgAttributes)))));
	});
var $author$project$Drawing2d$Attributes$groupAttributes = function (attributeValues) {
	return A2(
		$author$project$Drawing2d$Attributes$addTextAttributes,
		attributeValues,
		A2(
			$author$project$Drawing2d$Attributes$addFillStyle,
			attributeValues,
			A2(
				$author$project$Drawing2d$Attributes$addStrokeAttributes,
				attributeValues,
				A2($author$project$Drawing2d$Attributes$addGenericAttributes, attributeValues, _List_Nil))));
};
var $elm$svg$Svg$node = $elm$virtual_dom$VirtualDom$nodeNS('http://www.w3.org/2000/svg');
var $author$project$Drawing2d$render = F2(
	function (context, _v0) {
		var _function = _v0.a;
		return _function(context);
	});
var $author$project$Drawing2d$RenderContext$Protected$RenderContext = function (a) {
	return {$: 'RenderContext', a: a};
};
var $author$project$Drawing2d$RenderContext$Protected$update = F2(
	function (attributeValues, _v0) {
		var context = _v0.a;
		var updatedStrokeWidth = A2($elm$core$Maybe$withDefault, context.strokeWidth, attributeValues.strokeWidth);
		var updatedStrokeGradient = function () {
			var _v5 = attributeValues.strokeStyle;
			if (_v5.$ === 'Nothing') {
				return context.strokeGradient;
			} else {
				if (_v5.a.$ === 'StrokeColor') {
					return $elm$core$Maybe$Nothing;
				} else {
					var gradient = _v5.a.a;
					return $elm$core$Maybe$Just(gradient);
				}
			}
		}();
		var updatedFontSize = A2($elm$core$Maybe$withDefault, context.fontSize, attributeValues.fontSize);
		var updatedFillGradient = function () {
			var _v2 = attributeValues.fillStyle;
			if (_v2.$ === 'Nothing') {
				return context.fillGradient;
			} else {
				switch (_v2.a.$) {
					case 'NoFill':
						var _v3 = _v2.a;
						return $elm$core$Maybe$Nothing;
					case 'TransparentFill':
						var _v4 = _v2.a;
						return $elm$core$Maybe$Nothing;
					case 'FillColor':
						return $elm$core$Maybe$Nothing;
					default:
						var gradient = _v2.a.a;
						return $elm$core$Maybe$Just(gradient);
				}
			}
		}();
		var updatedDashPattern = function () {
			var _v1 = attributeValues.strokeDashPattern;
			if (_v1.$ === 'Nothing') {
				return context.strokeDashPattern;
			} else {
				var dashPattern = _v1.a;
				return dashPattern;
			}
		}();
		var updatedBordersVisible = A2($elm$core$Maybe$withDefault, context.bordersVisible, attributeValues.borderVisibility);
		return $author$project$Drawing2d$RenderContext$Protected$RenderContext(
			{bordersVisible: updatedBordersVisible, fillGradient: updatedFillGradient, fontSize: updatedFontSize, pixelSize: context.pixelSize, strokeDashPattern: updatedDashPattern, strokeGradient: updatedStrokeGradient, strokeWidth: updatedStrokeWidth});
	});
var $author$project$Drawing2d$groupLike = F4(
	function (tag, extraSvgAttributes, attributes, childEntities) {
		return $author$project$Drawing2d$Entity(
			function (currentContext) {
				var attributeValues = $author$project$Drawing2d$Attributes$collectAttributeValues(attributes);
				var childContext = A2($author$project$Drawing2d$RenderContext$Protected$update, attributeValues, currentContext);
				var childSvgElements = A2(
					$elm$core$List$map,
					$author$project$Drawing2d$render(childContext),
					childEntities);
				var defs = A2(
					$author$project$Drawing2d$addDropShadow,
					attributeValues,
					A2(
						$author$project$Drawing2d$addFillGradient,
						attributeValues,
						A2($author$project$Drawing2d$addStrokeGradient, attributeValues, _List_Nil)));
				var groupAttributes = $author$project$Drawing2d$Attributes$groupAttributes(attributeValues);
				return A3(
					$elm$svg$Svg$node,
					tag,
					_Utils_ap(groupAttributes, extraSvgAttributes),
					_Utils_ap(defs, childSvgElements));
			});
	});
var $author$project$Drawing2d$group = F2(
	function (attributes, childEntities) {
		return A4($author$project$Drawing2d$groupLike, 'g', _List_Nil, attributes, childEntities);
	});
var $ianmackenzie$elm_units$Quantity$zero = $ianmackenzie$elm_units$Quantity$Quantity(0);
var $author$project$Drawing2d$RenderContext$Protected$init = function (pixelSize) {
	return $author$project$Drawing2d$RenderContext$Protected$RenderContext(
		{bordersVisible: false, fillGradient: $elm$core$Maybe$Nothing, fontSize: $ianmackenzie$elm_units$Quantity$zero, pixelSize: pixelSize, strokeDashPattern: _List_Nil, strokeGradient: $elm$core$Maybe$Nothing, strokeWidth: $ianmackenzie$elm_units$Quantity$zero});
};
var $author$project$Drawing2d$Attributes$FillColor = function (a) {
	return {$: 'FillColor', a: a};
};
var $author$project$Drawing2d$Attributes$FillStyle = function (a) {
	return {$: 'FillStyle', a: a};
};
var $author$project$Drawing2d$noFill = $author$project$Drawing2d$Attributes$FillStyle(
	$author$project$Drawing2d$Attributes$FillColor('none'));
var $author$project$Drawing2d$Attributes$NoCap = {$: 'NoCap'};
var $author$project$Drawing2d$Attributes$StrokeLineCap = function (a) {
	return {$: 'StrokeLineCap', a: a};
};
var $author$project$Drawing2d$noStrokeCaps = $author$project$Drawing2d$Attributes$StrokeLineCap($author$project$Drawing2d$Attributes$NoCap);
var $ianmackenzie$elm_units$Quantity$per = F2(
	function (_v0, _v1) {
		var independentValue = _v0.a;
		var dependentValue = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(dependentValue / independentValue);
	});
var $ianmackenzie$elm_units$Pixels$pixels = function (numPixels) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numPixels);
};
var $ianmackenzie$elm_units$Pixels$pixel = $ianmackenzie$elm_units$Pixels$pixels(1);
var $ianmackenzie$elm_units$Pixels$toFloat = function (_v0) {
	var numPixels = _v0.a;
	return numPixels;
};
var $author$project$Drawing2d$px = function (value) {
	return $elm$core$String$fromFloat(
		$ianmackenzie$elm_units$Pixels$toFloat(value)) + 'px';
};
var $ianmackenzie$elm_geometry$Point2d$origin = $ianmackenzie$elm_geometry$Geometry$Types$Point2d(
	{x: 0, y: 0});
var $ianmackenzie$elm_geometry$Geometry$Types$Direction2d = function (a) {
	return {$: 'Direction2d', a: a};
};
var $ianmackenzie$elm_geometry$Direction2d$positiveX = $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
	{x: 1, y: 0});
var $ianmackenzie$elm_geometry$Direction2d$x = $ianmackenzie$elm_geometry$Direction2d$positiveX;
var $ianmackenzie$elm_geometry$Direction2d$positiveY = $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
	{x: 0, y: 1});
var $ianmackenzie$elm_geometry$Direction2d$y = $ianmackenzie$elm_geometry$Direction2d$positiveY;
var $ianmackenzie$elm_geometry$Frame2d$atOrigin = $ianmackenzie$elm_geometry$Geometry$Types$Frame2d(
	{originPoint: $ianmackenzie$elm_geometry$Point2d$origin, xDirection: $ianmackenzie$elm_geometry$Direction2d$x, yDirection: $ianmackenzie$elm_geometry$Direction2d$y});
var $author$project$Drawing2d$Attributes$Event = function (a) {
	return {$: 'Event', a: a};
};
var $author$project$Drawing2d$addGradientElements = F2(
	function (maybeGradient, svgElements) {
		if (maybeGradient.$ === 'Nothing') {
			return svgElements;
		} else {
			var gradient = maybeGradient.a;
			return A2($author$project$Drawing2d$Gradient$render, gradient, svgElements);
		}
	});
var $author$project$Drawing2d$addTransformedFillGradientReference = F2(
	function (maybeGradient, svgAttributes) {
		if (maybeGradient.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var gradient = maybeGradient.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$fill(
					$author$project$Drawing2d$Gradient$reference(gradient)),
				svgAttributes);
		}
	});
var $author$project$Drawing2d$addTransformedStrokeGradientReference = F2(
	function (maybeGradient, svgAttributes) {
		if (maybeGradient.$ === 'Nothing') {
			return svgAttributes;
		} else {
			var gradient = maybeGradient.a;
			return A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$stroke(
					$author$project$Drawing2d$Gradient$reference(gradient)),
				svgAttributes);
		}
	});
var $author$project$Drawing2d$RenderContext$Protected$fillGradient = function (_v0) {
	var context = _v0.a;
	return context.fillGradient;
};
var $ianmackenzie$elm_geometry$Direction2d$placeIn = F2(
	function (_v0, _v1) {
		var frame = _v0.a;
		var d = _v1.a;
		var _v2 = frame.yDirection;
		var dy = _v2.a;
		var _v3 = frame.xDirection;
		var dx = _v3.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
			{x: (d.x * dx.x) + (d.y * dy.x), y: (d.x * dx.y) + (d.y * dy.y)});
	});
var $ianmackenzie$elm_geometry$Point2d$placeIn = F2(
	function (_v0, _v1) {
		var frame = _v0.a;
		var p = _v1.a;
		var _v2 = frame.originPoint;
		var p0 = _v2.a;
		var _v3 = frame.yDirection;
		var j = _v3.a;
		var _v4 = frame.xDirection;
		var i = _v4.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point2d(
			{x: (p0.x + (p.x * i.x)) + (p.y * j.x), y: (p0.y + (p.x * i.y)) + (p.y * j.y)});
	});
var $ianmackenzie$elm_geometry$Frame2d$placeIn = F2(
	function (otherFrame, frame) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Frame2d(
			{
				originPoint: A2(
					$ianmackenzie$elm_geometry$Point2d$placeIn,
					otherFrame,
					$ianmackenzie$elm_geometry$Frame2d$originPoint(frame)),
				xDirection: A2(
					$ianmackenzie$elm_geometry$Direction2d$placeIn,
					otherFrame,
					$ianmackenzie$elm_geometry$Frame2d$xDirection(frame)),
				yDirection: A2(
					$ianmackenzie$elm_geometry$Direction2d$placeIn,
					otherFrame,
					$ianmackenzie$elm_geometry$Frame2d$yDirection(frame))
			});
	});
var $author$project$Drawing2d$placementTransform = function (frame) {
	var p = $ianmackenzie$elm_geometry$Point2d$unwrap(
		$ianmackenzie$elm_geometry$Frame2d$originPoint(frame));
	var j = $ianmackenzie$elm_geometry$Direction2d$unwrap(
		$ianmackenzie$elm_geometry$Frame2d$yDirection(frame));
	var i = $ianmackenzie$elm_geometry$Direction2d$unwrap(
		$ianmackenzie$elm_geometry$Frame2d$xDirection(frame));
	var matrixComponents = _List_fromArray(
		[
			$elm$core$String$fromFloat(i.x),
			$elm$core$String$fromFloat(-i.y),
			$elm$core$String$fromFloat(-j.x),
			$elm$core$String$fromFloat(j.y),
			$elm$core$String$fromFloat(p.x),
			$elm$core$String$fromFloat(-p.y)
		]);
	var transform = 'matrix(' + (A2($elm$core$String$join, ' ', matrixComponents) + ')');
	return $elm$svg$Svg$Attributes$transform(transform);
};
var $author$project$Drawing2d$Gradient$LinearGradient = function (a) {
	return {$: 'LinearGradient', a: a};
};
var $author$project$Drawing2d$Gradient$makeLinearGradient = F3(
	function (start, end, stops) {
		var p2 = $ianmackenzie$elm_geometry$Point2d$unwrap(end);
		var p1 = $ianmackenzie$elm_geometry$Point2d$unwrap(start);
		var id = A2(
			$elm$core$String$join,
			'_',
			_List_fromArray(
				[
					'lg',
					$elm$core$String$fromFloat(p1.x),
					$elm$core$String$fromFloat(p1.y),
					$elm$core$String$fromFloat(p2.x),
					$elm$core$String$fromFloat(p2.y),
					$author$project$Drawing2d$Stops$id(stops)
				]));
		return $author$project$Drawing2d$Gradient$LinearGradient(
			{end: end, id: id, start: start, stops: stops});
	});
var $author$project$Drawing2d$Gradient$RadialGradient = function (a) {
	return {$: 'RadialGradient', a: a};
};
var $author$project$Drawing2d$Gradient$makeRadialGradient = F3(
	function (start, end, stops) {
		var f = $ianmackenzie$elm_geometry$Point2d$unwrap(start);
		var c = $ianmackenzie$elm_geometry$Point2d$unwrap(
			$ianmackenzie$elm_geometry$Circle2d$centerPoint(end));
		var _v0 = $ianmackenzie$elm_geometry$Circle2d$radius(end);
		var r = _v0.a;
		var id = A2(
			$elm$core$String$join,
			'_',
			_List_fromArray(
				[
					'rg',
					$elm$core$String$fromFloat(f.x),
					$elm$core$String$fromFloat(f.y),
					$elm$core$String$fromFloat(c.x),
					$elm$core$String$fromFloat(c.y),
					$elm$core$String$fromFloat(r),
					$author$project$Drawing2d$Stops$id(stops)
				]));
		return $author$project$Drawing2d$Gradient$RadialGradient(
			{end: end, id: id, start: start, stops: stops});
	});
var $ianmackenzie$elm_geometry$Point2d$relativeTo = F2(
	function (_v0, _v1) {
		var frame = _v0.a;
		var p = _v1.a;
		var _v2 = frame.originPoint;
		var p0 = _v2.a;
		var deltaX = p.x - p0.x;
		var deltaY = p.y - p0.y;
		var _v3 = frame.yDirection;
		var j = _v3.a;
		var _v4 = frame.xDirection;
		var i = _v4.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point2d(
			{x: (deltaX * i.x) + (deltaY * i.y), y: (deltaX * j.x) + (deltaY * j.y)});
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Circle2d = function (a) {
	return {$: 'Circle2d', a: a};
};
var $ianmackenzie$elm_units$Quantity$abs = function (_v0) {
	var value = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(
		$elm$core$Basics$abs(value));
};
var $ianmackenzie$elm_geometry$Circle2d$withRadius = F2(
	function (givenRadius, givenCenterPoint) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Circle2d(
			{
				centerPoint: givenCenterPoint,
				radius: $ianmackenzie$elm_units$Quantity$abs(givenRadius)
			});
	});
var $ianmackenzie$elm_geometry$Circle2d$relativeTo = F2(
	function (frame, _v0) {
		var circle = _v0.a;
		return A2(
			$ianmackenzie$elm_geometry$Circle2d$withRadius,
			circle.radius,
			A2($ianmackenzie$elm_geometry$Point2d$relativeTo, frame, circle.centerPoint));
	});
var $author$project$Drawing2d$Gradient$relativeTo = F2(
	function (frame, gradient) {
		if (gradient.$ === 'LinearGradient') {
			var linearGradient = gradient.a;
			return A3(
				$author$project$Drawing2d$Gradient$makeLinearGradient,
				A2($ianmackenzie$elm_geometry$Point2d$relativeTo, frame, linearGradient.start),
				A2($ianmackenzie$elm_geometry$Point2d$relativeTo, frame, linearGradient.end),
				linearGradient.stops);
		} else {
			var radialGradient = gradient.a;
			return A3(
				$author$project$Drawing2d$Gradient$makeRadialGradient,
				A2($ianmackenzie$elm_geometry$Point2d$relativeTo, frame, radialGradient.start),
				A2($ianmackenzie$elm_geometry$Circle2d$relativeTo, frame, radialGradient.end),
				radialGradient.stops);
		}
	});
var $author$project$Drawing2d$RenderContext$Protected$relativeTo = F2(
	function (frame, _v0) {
		var context = _v0.a;
		return $author$project$Drawing2d$RenderContext$Protected$RenderContext(
			{
				bordersVisible: context.bordersVisible,
				fillGradient: A2(
					$elm$core$Maybe$map,
					$author$project$Drawing2d$Gradient$relativeTo(frame),
					context.fillGradient),
				fontSize: context.fontSize,
				pixelSize: context.pixelSize,
				strokeDashPattern: context.strokeDashPattern,
				strokeGradient: A2(
					$elm$core$Maybe$map,
					$author$project$Drawing2d$Gradient$relativeTo(frame),
					context.strokeGradient),
				strokeWidth: context.strokeWidth
			});
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Rectangle2d = function (a) {
	return {$: 'Rectangle2d', a: a};
};
var $ianmackenzie$elm_geometry$Direction2d$relativeTo = F2(
	function (_v0, _v1) {
		var frame = _v0.a;
		var d = _v1.a;
		var _v2 = frame.yDirection;
		var dy = _v2.a;
		var _v3 = frame.xDirection;
		var dx = _v3.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
			{x: (d.x * dx.x) + (d.y * dx.y), y: (d.x * dy.x) + (d.y * dy.y)});
	});
var $ianmackenzie$elm_geometry$Frame2d$relativeTo = F2(
	function (otherFrame, frame) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Frame2d(
			{
				originPoint: A2(
					$ianmackenzie$elm_geometry$Point2d$relativeTo,
					otherFrame,
					$ianmackenzie$elm_geometry$Frame2d$originPoint(frame)),
				xDirection: A2(
					$ianmackenzie$elm_geometry$Direction2d$relativeTo,
					otherFrame,
					$ianmackenzie$elm_geometry$Frame2d$xDirection(frame)),
				yDirection: A2(
					$ianmackenzie$elm_geometry$Direction2d$relativeTo,
					otherFrame,
					$ianmackenzie$elm_geometry$Frame2d$yDirection(frame))
			});
	});
var $ianmackenzie$elm_geometry$Rectangle2d$relativeTo = F2(
	function (frame, rectangle) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Rectangle2d(
			{
				axes: A2(
					$ianmackenzie$elm_geometry$Frame2d$relativeTo,
					frame,
					$ianmackenzie$elm_geometry$Rectangle2d$axes(rectangle)),
				dimensions: $ianmackenzie$elm_geometry$Rectangle2d$dimensions(rectangle)
			});
	});
var $author$project$Drawing2d$RenderContext$Protected$strokeGradient = function (_v0) {
	var context = _v0.a;
	return context.strokeGradient;
};
var $author$project$Drawing2d$placeImpl = F3(
	function (transformationFrame, coordinateConversionFrame, _v0) {
		var _function = _v0.a;
		return $author$project$Drawing2d$Entity(
			function (currentContext) {
				var transformEvent = function (_v2) {
					var callback = _v2.a;
					return $author$project$Drawing2d$Attributes$Event(
						A2(
							$elm$core$Basics$composeR,
							$ianmackenzie$elm_geometry$Rectangle2d$relativeTo(coordinateConversionFrame),
							callback));
				};
				var transform = $author$project$Drawing2d$placementTransform(
					A2($ianmackenzie$elm_geometry$Frame2d$placeIn, coordinateConversionFrame, transformationFrame));
				var localContext = A2(
					$author$project$Drawing2d$RenderContext$Protected$relativeTo,
					A2($ianmackenzie$elm_geometry$Frame2d$placeIn, coordinateConversionFrame, transformationFrame),
					currentContext);
				var localFillGradient = $author$project$Drawing2d$RenderContext$Protected$fillGradient(localContext);
				var localStrokeGradient = $author$project$Drawing2d$RenderContext$Protected$strokeGradient(localContext);
				var localGradientElements = A2(
					$author$project$Drawing2d$addGradientElements,
					localStrokeGradient,
					A2($author$project$Drawing2d$addGradientElements, localFillGradient, _List_Nil));
				var localGradientReferences = A2(
					$author$project$Drawing2d$addTransformedStrokeGradientReference,
					localStrokeGradient,
					A2($author$project$Drawing2d$addTransformedFillGradientReference, localFillGradient, _List_Nil));
				var localSvgElement = _function(localContext);
				var localElement = function () {
					if (!localGradientElements.b) {
						return localSvgElement;
					} else {
						return A2(
							$elm$svg$Svg$g,
							localGradientReferences,
							A2($elm$core$List$cons, localSvgElement, localGradientElements));
					}
				}();
				var groupElement = A2(
					$elm$svg$Svg$g,
					_List_fromArray(
						[transform]),
					_List_fromArray(
						[localElement]));
				return A2($elm$svg$Svg$map, transformEvent, groupElement);
			});
	});
var $author$project$Drawing2d$placeIn = F2(
	function (frame, entity) {
		return A3($author$project$Drawing2d$placeImpl, $ianmackenzie$elm_geometry$Frame2d$atOrigin, frame, entity);
	});
var $author$project$Drawing2d$relativeTo = F2(
	function (frame, entity) {
		return A2(
			$author$project$Drawing2d$placeIn,
			A2($ianmackenzie$elm_geometry$Frame2d$relativeTo, frame, $ianmackenzie$elm_geometry$Frame2d$atOrigin),
			entity);
	});
var $author$project$Drawing2d$Attributes$StrokeWidth = function (a) {
	return {$: 'StrokeWidth', a: a};
};
var $author$project$Drawing2d$strokeWidth = function (givenWidth) {
	return $author$project$Drawing2d$Attributes$StrokeWidth(givenWidth);
};
var $author$project$Drawing2d$Attributes$BorderVisibility = function (a) {
	return {$: 'BorderVisibility', a: a};
};
var $author$project$Drawing2d$strokedBorder = $author$project$Drawing2d$Attributes$BorderVisibility(true);
var $author$project$Drawing2d$svgStaticCss = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2($elm$html$Html$Attributes$style, 'height', '100%'),
		A2($elm$html$Html$Attributes$style, 'width', '100%'),
		A2($elm$html$Html$Attributes$style, 'left', '0'),
		A2($elm$html$Html$Attributes$style, 'right', '0')
	]);
var $author$project$Drawing2d$custom = function (given) {
	var topLevelViewBox = A2(
		$ianmackenzie$elm_geometry$Rectangle2d$relativeTo,
		$ianmackenzie$elm_geometry$Rectangle2d$axes(given.viewBox),
		given.viewBox);
	var defaultAttributes = _List_fromArray(
		[
			$author$project$Drawing2d$blackStroke,
			$author$project$Drawing2d$strokeWidth(given.strokeWidth),
			$author$project$Drawing2d$bevelStrokeJoins,
			$author$project$Drawing2d$noStrokeCaps,
			$author$project$Drawing2d$noFill,
			$author$project$Drawing2d$strokedBorder,
			$author$project$Drawing2d$fontSize(given.fontSize),
			$author$project$Drawing2d$blackText,
			$author$project$Drawing2d$anchorAtStart,
			$author$project$Drawing2d$alphabeticBaseline
		]);
	var _v0 = $ianmackenzie$elm_geometry$Rectangle2d$dimensions(given.viewBox);
	var viewBoxWidth = _v0.a;
	var viewBoxHeight = _v0.b;
	var drawingResolution = function () {
		var _v3 = given.size;
		switch (_v3.$) {
			case 'Scale':
				var factor = _v3.a;
				return factor;
			case 'Width':
				var value = _v3.a;
				return A2($ianmackenzie$elm_units$Quantity$per, viewBoxWidth, value);
			default:
				var value = _v3.a;
				return A2($ianmackenzie$elm_units$Quantity$per, viewBoxHeight, value);
		}
	}();
	var canvasHeight = A2($ianmackenzie$elm_units$Quantity$at, drawingResolution, viewBoxHeight);
	var canvasWidth = A2($ianmackenzie$elm_units$Quantity$at, drawingResolution, viewBoxWidth);
	var containerSizeCss = _List_fromArray(
		[
			A2(
			$elm$html$Html$Attributes$style,
			'width',
			$author$project$Drawing2d$px(canvasWidth)),
			A2(
			$elm$html$Html$Attributes$style,
			'height',
			$author$project$Drawing2d$px(canvasHeight))
		]);
	var topLevelPixelSize = A2($ianmackenzie$elm_units$Quantity$at_, drawingResolution, $ianmackenzie$elm_units$Pixels$pixel);
	var initialRenderContext = $author$project$Drawing2d$RenderContext$Protected$init(topLevelPixelSize);
	var viewBoxAttribute = $elm$svg$Svg$Attributes$viewBox(
		A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					$elm$core$String$fromFloat(
					(-0.5) * $ianmackenzie$elm_units$Quantity$unwrap(viewBoxWidth)),
					$elm$core$String$fromFloat(
					(-0.5) * $ianmackenzie$elm_units$Quantity$unwrap(viewBoxHeight)),
					$elm$core$String$fromFloat(
					$ianmackenzie$elm_units$Quantity$unwrap(viewBoxWidth)),
					$elm$core$String$fromFloat(
					$ianmackenzie$elm_units$Quantity$unwrap(viewBoxHeight))
				])));
	var _v1 = A4(
		$author$project$Drawing2d$groupLike,
		'svg',
		A2($elm$core$List$cons, viewBoxAttribute, $author$project$Drawing2d$svgStaticCss),
		defaultAttributes,
		_List_fromArray(
			[
				A2(
				$author$project$Drawing2d$relativeTo,
				$ianmackenzie$elm_geometry$Rectangle2d$axes(given.viewBox),
				A2($author$project$Drawing2d$group, _List_Nil, given.entities))
			]));
	var svgElement = _v1.a;
	return A2(
		$elm$html$Html$div,
		_Utils_ap($author$project$Drawing2d$containerStaticCss, containerSizeCss),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$map,
				function (_v2) {
					var callback = _v2.a;
					return callback(topLevelViewBox);
				},
				svgElement(initialRenderContext))
			]));
};
var $author$project$Drawing2d$Scale = function (a) {
	return {$: 'Scale', a: a};
};
var $author$project$Drawing2d$fixed = $author$project$Drawing2d$Scale(
	$ianmackenzie$elm_units$Quantity$Quantity(1));
var $ianmackenzie$elm_units$Pixels$float = function (numPixels) {
	return $ianmackenzie$elm_units$Quantity$Quantity(numPixels);
};
var $author$project$Drawing2d$draw = function (_arguments) {
	return $author$project$Drawing2d$custom(
		{
			entities: _arguments.entities,
			fontSize: $ianmackenzie$elm_units$Pixels$float(16),
			size: $author$project$Drawing2d$fixed,
			strokeWidth: $ianmackenzie$elm_units$Pixels$float(1),
			viewBox: _arguments.viewBox
		});
};
var $ianmackenzie$elm_geometry$Point2d$pixels = F2(
	function (x, y) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Point2d(
			{x: x, y: y});
	});
var $author$project$ModelSystem$Level$extendP2d = F3(
	function (p, dx, dy) {
		var py = $ianmackenzie$elm_units$Pixels$toFloat(
			$ianmackenzie$elm_geometry$Point2d$yCoordinate(p));
		var px = $ianmackenzie$elm_units$Pixels$toFloat(
			$ianmackenzie$elm_geometry$Point2d$xCoordinate(p));
		return A2($ianmackenzie$elm_geometry$Point2d$pixels, px + dx, py + dy);
	});
var $avh4$elm_color$Color$toCssString = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	var roundTo = function (x) {
		return $elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return $elm$core$Basics$round(x * 10000) / 100;
	};
	return $elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				$elm$core$String$fromFloat(
				pct(r)),
				'%,',
				$elm$core$String$fromFloat(
				pct(g)),
				'%,',
				$elm$core$String$fromFloat(
				pct(b)),
				'%,',
				$elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var $author$project$Drawing2d$fillColor = function (color) {
	return $author$project$Drawing2d$Attributes$FillStyle(
		$author$project$Drawing2d$Attributes$FillColor(
			$avh4$elm_color$Color$toCssString(color)));
};
var $ianmackenzie$elm_geometry$Geometry$Types$Arc2d = function (a) {
	return {$: 'Arc2d', a: a};
};
var $ianmackenzie$elm_geometry$Vector2d$direction = function (_v0) {
	var v = _v0.a;
	var largestComponent = A2(
		$elm$core$Basics$max,
		$elm$core$Basics$abs(v.x),
		$elm$core$Basics$abs(v.y));
	if (!largestComponent) {
		return $elm$core$Maybe$Nothing;
	} else {
		var scaledY = v.y / largestComponent;
		var scaledX = v.x / largestComponent;
		var scaledLength = $elm$core$Basics$sqrt((scaledX * scaledX) + (scaledY * scaledY));
		return $elm$core$Maybe$Just(
			$ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
				{x: scaledX / scaledLength, y: scaledY / scaledLength}));
	}
};
var $ianmackenzie$elm_geometry$Geometry$Types$Vector2d = function (a) {
	return {$: 'Vector2d', a: a};
};
var $ianmackenzie$elm_geometry$Vector2d$from = F2(
	function (_v0, _v1) {
		var p1 = _v0.a;
		var p2 = _v1.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Vector2d(
			{x: p2.x - p1.x, y: p2.y - p1.y});
	});
var $ianmackenzie$elm_geometry$Vector2d$length = function (_v0) {
	var v = _v0.a;
	var largestComponent = A2(
		$elm$core$Basics$max,
		$elm$core$Basics$abs(v.x),
		$elm$core$Basics$abs(v.y));
	if (!largestComponent) {
		return $ianmackenzie$elm_units$Quantity$zero;
	} else {
		var scaledY = v.y / largestComponent;
		var scaledX = v.x / largestComponent;
		var scaledLength = $elm$core$Basics$sqrt((scaledX * scaledX) + (scaledY * scaledY));
		return $ianmackenzie$elm_units$Quantity$Quantity(scaledLength * largestComponent);
	}
};
var $ianmackenzie$elm_geometry$Quantity$Extra$rTheta = F2(
	function (_v0, _v1) {
		var r = _v0.a;
		var theta = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(r * theta);
	});
var $ianmackenzie$elm_units$Quantity$ratio = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v1.a;
		return x / y;
	});
var $ianmackenzie$elm_geometry$Direction2d$rotateBy = F2(
	function (_v0, _v1) {
		var angle = _v0.a;
		var d = _v1.a;
		var s = $elm$core$Basics$sin(angle);
		var c = $elm$core$Basics$cos(angle);
		return $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
			{x: (c * d.x) - (s * d.y), y: (s * d.x) + (c * d.y)});
	});
var $ianmackenzie$elm_units$Angle$sin = function (_v0) {
	var angle = _v0.a;
	return $elm$core$Basics$sin(angle);
};
var $ianmackenzie$elm_geometry$Arc2d$twoPi = $ianmackenzie$elm_units$Angle$radians(2 * $elm$core$Basics$pi);
var $ianmackenzie$elm_geometry$Arc2d$from = F3(
	function (givenStartPoint, givenEndPoint, givenSweptAngle) {
		var displacement = A2($ianmackenzie$elm_geometry$Vector2d$from, givenStartPoint, givenEndPoint);
		var _v0 = $ianmackenzie$elm_geometry$Vector2d$direction(displacement);
		if (_v0.$ === 'Just') {
			var direction = _v0.a;
			var numTurns = A2($ianmackenzie$elm_units$Quantity$ratio, givenSweptAngle, $ianmackenzie$elm_geometry$Arc2d$twoPi);
			var halfAngle = A2($ianmackenzie$elm_units$Quantity$multiplyBy, 0.5, givenSweptAngle);
			var scale = 1 / (2 * $elm$core$Basics$abs(
				$ianmackenzie$elm_units$Angle$sin(halfAngle)));
			var distance = $ianmackenzie$elm_geometry$Vector2d$length(displacement);
			var computedRadius = A2($ianmackenzie$elm_units$Quantity$multiplyBy, scale, distance);
			var angleModTwoPi = A2(
				$ianmackenzie$elm_units$Quantity$minus,
				A2(
					$ianmackenzie$elm_units$Quantity$multiplyBy,
					$elm$core$Basics$floor(numTurns),
					$ianmackenzie$elm_geometry$Arc2d$twoPi),
				givenSweptAngle);
			return $ianmackenzie$elm_geometry$Geometry$Types$Arc2d(
				{
					signedLength: _Utils_eq(givenSweptAngle, $ianmackenzie$elm_units$Quantity$zero) ? distance : A2($ianmackenzie$elm_geometry$Quantity$Extra$rTheta, computedRadius, givenSweptAngle),
					startPoint: givenStartPoint,
					sweptAngle: givenSweptAngle,
					xDirection: A2(
						$ianmackenzie$elm_geometry$Direction2d$rotateBy,
						A2($ianmackenzie$elm_units$Quantity$multiplyBy, -0.5, angleModTwoPi),
						direction)
				});
		} else {
			return $ianmackenzie$elm_geometry$Geometry$Types$Arc2d(
				{signedLength: $ianmackenzie$elm_units$Quantity$zero, startPoint: givenStartPoint, sweptAngle: givenSweptAngle, xDirection: $ianmackenzie$elm_geometry$Direction2d$x});
		}
	});
var $ianmackenzie$elm_units$Quantity$greaterThanOrEqualTo = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return _Utils_cmp(x, y) > -1;
	});
var $ianmackenzie$elm_units$Quantity$midpoint = F2(
	function (_v0, _v1) {
		var x = _v0.a;
		var y = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity(x + (0.5 * (y - x)));
	});
var $ianmackenzie$elm_geometry$Direction2d$negativeX = $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
	{x: -1, y: 0});
var $ianmackenzie$elm_geometry$Direction2d$negativeY = $ianmackenzie$elm_geometry$Geometry$Types$Direction2d(
	{x: 0, y: -1});
var $ianmackenzie$elm_geometry$Frame2d$unsafe = function (properties) {
	return $ianmackenzie$elm_geometry$Geometry$Types$Frame2d(properties);
};
var $ianmackenzie$elm_geometry$Rectangle2d$axisAligned = F4(
	function (x1, y1, x2, y2) {
		var computedYDirection = A2($ianmackenzie$elm_units$Quantity$greaterThanOrEqualTo, y1, y2) ? $ianmackenzie$elm_geometry$Direction2d$positiveY : $ianmackenzie$elm_geometry$Direction2d$negativeY;
		var computedXDirection = A2($ianmackenzie$elm_units$Quantity$greaterThanOrEqualTo, x1, x2) ? $ianmackenzie$elm_geometry$Direction2d$positiveX : $ianmackenzie$elm_geometry$Direction2d$negativeX;
		var computedDimensions = _Utils_Tuple2(
			$ianmackenzie$elm_units$Quantity$abs(
				A2($ianmackenzie$elm_units$Quantity$minus, x1, x2)),
			$ianmackenzie$elm_units$Quantity$abs(
				A2($ianmackenzie$elm_units$Quantity$minus, y1, y2)));
		var computedCenterPoint = A2(
			$ianmackenzie$elm_geometry$Point2d$xy,
			A2($ianmackenzie$elm_units$Quantity$midpoint, x1, x2),
			A2($ianmackenzie$elm_units$Quantity$midpoint, y1, y2));
		var computedAxes = $ianmackenzie$elm_geometry$Frame2d$unsafe(
			{originPoint: computedCenterPoint, xDirection: computedXDirection, yDirection: computedYDirection});
		return $ianmackenzie$elm_geometry$Geometry$Types$Rectangle2d(
			{axes: computedAxes, dimensions: computedDimensions});
	});
var $ianmackenzie$elm_geometry$Rectangle2d$from = F2(
	function (p1, p2) {
		return A4(
			$ianmackenzie$elm_geometry$Rectangle2d$axisAligned,
			$ianmackenzie$elm_geometry$Point2d$xCoordinate(p1),
			$ianmackenzie$elm_geometry$Point2d$yCoordinate(p1),
			$ianmackenzie$elm_geometry$Point2d$xCoordinate(p2),
			$ianmackenzie$elm_geometry$Point2d$yCoordinate(p2));
	});
var $ianmackenzie$elm_geometry$BoundingBox2d$extrema = function (_v0) {
	var boundingBoxExtrema = _v0.a;
	return boundingBoxExtrema;
};
var $ianmackenzie$elm_geometry$Rectangle2d$fromBoundingBox = function (givenBox) {
	var _v0 = $ianmackenzie$elm_geometry$BoundingBox2d$extrema(givenBox);
	var minX = _v0.minX;
	var maxX = _v0.maxX;
	var minY = _v0.minY;
	var maxY = _v0.maxY;
	return A4($ianmackenzie$elm_geometry$Rectangle2d$axisAligned, minX, minY, maxX, maxY);
};
var $avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 'RgbaSpace', a: a, b: b, c: c, d: d};
	});
var $avh4$elm_color$Color$grey = A4($avh4$elm_color$Color$RgbaSpace, 211 / 255, 215 / 255, 207 / 255, 1.0);
var $avh4$elm_color$Color$lightBlue = A4($avh4$elm_color$Color$RgbaSpace, 114 / 255, 159 / 255, 207 / 255, 1.0);
var $author$project$Drawing2d$addDefs = F2(
	function (defs, svgElement) {
		if (!defs.b) {
			return svgElement;
		} else {
			return A2(
				$elm$svg$Svg$g,
				_List_Nil,
				A2($elm$core$List$cons, svgElement, defs));
		}
	});
var $author$project$Drawing2d$RenderContext$Protected$bordersVisible = function (_v0) {
	var context = _v0.a;
	return context.bordersVisible;
};
var $author$project$Drawing2d$Attributes$noStroke = $elm$svg$Svg$Attributes$stroke('none');
var $author$project$Drawing2d$Attributes$regionAttributes = F2(
	function (bordersVisible, attributeValues) {
		var commonAttributes = A2(
			$author$project$Drawing2d$Attributes$addFillStyle,
			attributeValues,
			A2($author$project$Drawing2d$Attributes$addGenericAttributes, attributeValues, _List_Nil));
		return bordersVisible ? A2($author$project$Drawing2d$Attributes$addStrokeAttributes, attributeValues, commonAttributes) : A2($elm$core$List$cons, $author$project$Drawing2d$Attributes$noStroke, commonAttributes);
	});
var $author$project$Drawing2d$drawRegion = F3(
	function (attributes, renderer, region) {
		var attributeValues = $author$project$Drawing2d$Attributes$collectAttributeValues(attributes);
		return $author$project$Drawing2d$Entity(
			function (context) {
				var fillGradientElement = A2($author$project$Drawing2d$addFillGradient, attributeValues, _List_Nil);
				var bordersVisible = A2(
					$elm$core$Maybe$withDefault,
					$author$project$Drawing2d$RenderContext$Protected$bordersVisible(context),
					attributeValues.borderVisibility);
				var gradientElements = bordersVisible ? A2($author$project$Drawing2d$addStrokeGradient, attributeValues, fillGradientElement) : fillGradientElement;
				var defs = A2($author$project$Drawing2d$addDropShadow, attributeValues, gradientElements);
				var svgAttributes = A2($author$project$Drawing2d$Attributes$regionAttributes, bordersVisible, attributeValues);
				var regionElement = A2(renderer, svgAttributes, region);
				return A2($author$project$Drawing2d$addDefs, defs, regionElement);
			});
	});
var $ianmackenzie$elm_geometry$Polygon2d$innerLoops = function (_v0) {
	var polygon = _v0.a;
	return polygon.innerLoops;
};
var $ianmackenzie$elm_geometry$Polygon2d$outerLoop = function (_v0) {
	var polygon = _v0.a;
	return polygon.outerLoop;
};
var $author$project$Drawing2d$Svg$unsafePath = F2(
	function (svgAttributes, pathString) {
		return A2(
			$elm$svg$Svg$path,
			A2(
				$elm$core$List$cons,
				$elm$svg$Svg$Attributes$d(pathString),
				svgAttributes),
			_List_Nil);
	});
var $author$project$Drawing2d$Svg$polygon2d = F2(
	function (attributes, polygon) {
		var loops = A2(
			$elm$core$List$cons,
			$ianmackenzie$elm_geometry$Polygon2d$outerLoop(polygon),
			$ianmackenzie$elm_geometry$Polygon2d$innerLoops(polygon));
		var loopString = function (loop) {
			if (!loop.b) {
				return '';
			} else {
				var coordinateStrings = A2(
					$elm$core$List$map,
					function (point) {
						var _v1 = $ianmackenzie$elm_geometry$Point2d$unwrap(point);
						var x = _v1.x;
						var y = _v1.y;
						return $elm$core$String$fromFloat(x) + (' ' + $elm$core$String$fromFloat(-y));
					},
					loop);
				return 'M ' + (A2($elm$core$String$join, ' L ', coordinateStrings) + ' Z');
			}
		};
		return A2(
			$author$project$Drawing2d$Svg$unsafePath,
			attributes,
			A2(
				$elm$core$String$join,
				' ',
				A2($elm$core$List$map, loopString, loops)));
	});
var $author$project$Drawing2d$polygon = F2(
	function (attributes, givenPolygon) {
		return A3($author$project$Drawing2d$drawRegion, attributes, $author$project$Drawing2d$Svg$polygon2d, givenPolygon);
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Polygon2d = function (a) {
	return {$: 'Polygon2d', a: a};
};
var $ianmackenzie$elm_geometry$Vector2d$cross = F2(
	function (_v0, _v1) {
		var v2 = _v0.a;
		var v1 = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity((v1.x * v2.y) - (v1.y * v2.x));
	});
var $ianmackenzie$elm_geometry$Triangle2d$vertices = function (_v0) {
	var triangleVertices = _v0.a;
	return triangleVertices;
};
var $ianmackenzie$elm_geometry$Triangle2d$counterclockwiseArea = function (triangle) {
	var _v0 = $ianmackenzie$elm_geometry$Triangle2d$vertices(triangle);
	var p1 = _v0.a;
	var p2 = _v0.b;
	var p3 = _v0.c;
	var firstVector = A2($ianmackenzie$elm_geometry$Vector2d$from, p1, p2);
	var secondVector = A2($ianmackenzie$elm_geometry$Vector2d$from, p1, p3);
	return A2(
		$ianmackenzie$elm_units$Quantity$multiplyBy,
		0.5,
		A2($ianmackenzie$elm_geometry$Vector2d$cross, secondVector, firstVector));
};
var $ianmackenzie$elm_geometry$Geometry$Types$Triangle2d = function (a) {
	return {$: 'Triangle2d', a: a};
};
var $ianmackenzie$elm_geometry$Triangle2d$from = F3(
	function (p1, p2, p3) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Triangle2d(
			_Utils_Tuple3(p1, p2, p3));
	});
var $ianmackenzie$elm_units$Quantity$sum = function (quantities) {
	return A3($elm$core$List$foldl, $ianmackenzie$elm_units$Quantity$plus, $ianmackenzie$elm_units$Quantity$zero, quantities);
};
var $ianmackenzie$elm_geometry$Polygon2d$counterclockwiseArea = function (vertices_) {
	if (!vertices_.b) {
		return $ianmackenzie$elm_units$Quantity$zero;
	} else {
		if (!vertices_.b.b) {
			var single = vertices_.a;
			return $ianmackenzie$elm_units$Quantity$zero;
		} else {
			if (!vertices_.b.b.b) {
				var first = vertices_.a;
				var _v1 = vertices_.b;
				var second = _v1.a;
				return $ianmackenzie$elm_units$Quantity$zero;
			} else {
				var first = vertices_.a;
				var _v2 = vertices_.b;
				var second = _v2.a;
				var rest = _v2.b;
				var segmentArea = F2(
					function (start, end) {
						return $ianmackenzie$elm_geometry$Triangle2d$counterclockwiseArea(
							A3($ianmackenzie$elm_geometry$Triangle2d$from, first, start, end));
					});
				var segmentAreas = A3(
					$elm$core$List$map2,
					segmentArea,
					A2($elm$core$List$cons, second, rest),
					rest);
				return $ianmackenzie$elm_units$Quantity$sum(segmentAreas);
			}
		}
	}
};
var $ianmackenzie$elm_units$Quantity$lessThanOrEqualTo = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return _Utils_cmp(x, y) < 1;
	});
var $ianmackenzie$elm_geometry$Polygon2d$makeInnerLoop = function (vertices_) {
	return A2(
		$ianmackenzie$elm_units$Quantity$lessThanOrEqualTo,
		$ianmackenzie$elm_units$Quantity$zero,
		$ianmackenzie$elm_geometry$Polygon2d$counterclockwiseArea(vertices_)) ? vertices_ : $elm$core$List$reverse(vertices_);
};
var $ianmackenzie$elm_geometry$Polygon2d$makeOuterLoop = function (vertices_) {
	return A2(
		$ianmackenzie$elm_units$Quantity$greaterThanOrEqualTo,
		$ianmackenzie$elm_units$Quantity$zero,
		$ianmackenzie$elm_geometry$Polygon2d$counterclockwiseArea(vertices_)) ? vertices_ : $elm$core$List$reverse(vertices_);
};
var $ianmackenzie$elm_geometry$Polygon2d$withHoles = F2(
	function (givenInnerLoops, givenOuterLoop) {
		return $ianmackenzie$elm_geometry$Geometry$Types$Polygon2d(
			{
				innerLoops: A2($elm$core$List$map, $ianmackenzie$elm_geometry$Polygon2d$makeInnerLoop, givenInnerLoops),
				outerLoop: $ianmackenzie$elm_geometry$Polygon2d$makeOuterLoop(givenOuterLoop)
			});
	});
var $ianmackenzie$elm_geometry$Polygon2d$singleLoop = function (givenOuterLoop) {
	return A2($ianmackenzie$elm_geometry$Polygon2d$withHoles, _List_Nil, givenOuterLoop);
};
var $ianmackenzie$elm_units$Quantity$half = function (_v0) {
	var value = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(0.5 * value);
};
var $ianmackenzie$elm_units$Quantity$negate = function (_v0) {
	var value = _v0.a;
	return $ianmackenzie$elm_units$Quantity$Quantity(-value);
};
var $ianmackenzie$elm_geometry$Point2d$xyIn = F3(
	function (_v0, _v1, _v2) {
		var frame = _v0.a;
		var x = _v1.a;
		var y = _v2.a;
		var _v3 = frame.originPoint;
		var p0 = _v3.a;
		var _v4 = frame.yDirection;
		var j = _v4.a;
		var _v5 = frame.xDirection;
		var i = _v5.a;
		return $ianmackenzie$elm_geometry$Geometry$Types$Point2d(
			{x: (p0.x + (x * i.x)) + (y * j.x), y: (p0.y + (x * i.y)) + (y * j.y)});
	});
var $ianmackenzie$elm_geometry$Rectangle2d$vertices = function (rectangle) {
	var localFrame = $ianmackenzie$elm_geometry$Rectangle2d$axes(rectangle);
	var _v0 = $ianmackenzie$elm_geometry$Rectangle2d$dimensions(rectangle);
	var width = _v0.a;
	var height = _v0.b;
	var y = $ianmackenzie$elm_units$Quantity$half(height);
	var x = $ianmackenzie$elm_units$Quantity$half(width);
	return _List_fromArray(
		[
			A3(
			$ianmackenzie$elm_geometry$Point2d$xyIn,
			localFrame,
			$ianmackenzie$elm_units$Quantity$negate(x),
			$ianmackenzie$elm_units$Quantity$negate(y)),
			A3(
			$ianmackenzie$elm_geometry$Point2d$xyIn,
			localFrame,
			x,
			$ianmackenzie$elm_units$Quantity$negate(y)),
			A3($ianmackenzie$elm_geometry$Point2d$xyIn, localFrame, x, y),
			A3(
			$ianmackenzie$elm_geometry$Point2d$xyIn,
			localFrame,
			$ianmackenzie$elm_units$Quantity$negate(x),
			y)
		]);
};
var $ianmackenzie$elm_geometry$Rectangle2d$toPolygon = function (rectangle) {
	return $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
		$ianmackenzie$elm_geometry$Rectangle2d$vertices(rectangle));
};
var $author$project$Drawing2d$Svg$rectangle2d = F2(
	function (attributes, rectangle) {
		return A2(
			$author$project$Drawing2d$Svg$polygon2d,
			attributes,
			$ianmackenzie$elm_geometry$Rectangle2d$toPolygon(rectangle));
	});
var $author$project$Drawing2d$rectangle = F2(
	function (attributes, givenRectangle) {
		return A3($author$project$Drawing2d$drawRegion, attributes, $author$project$Drawing2d$Svg$rectangle2d, givenRectangle);
	});
var $ianmackenzie$elm_geometry$Geometry$Types$Polyline2d = function (a) {
	return {$: 'Polyline2d', a: a};
};
var $ianmackenzie$elm_geometry$Polyline2d$fromVertices = function (givenVertices) {
	return $ianmackenzie$elm_geometry$Geometry$Types$Polyline2d(givenVertices);
};
var $ianmackenzie$elm_geometry$Quantity$Extra$aXbY = F4(
	function (a, _v0, b, _v1) {
		var x = _v0.a;
		var y = _v1.a;
		return $ianmackenzie$elm_units$Quantity$Quantity((a * x) + (b * y));
	});
var $ianmackenzie$elm_units$Angle$cos = function (_v0) {
	var angle = _v0.a;
	return $elm$core$Basics$cos(angle);
};
var $ianmackenzie$elm_units$Quantity$lessThan = F2(
	function (_v0, _v1) {
		var y = _v0.a;
		var x = _v1.a;
		return _Utils_cmp(x, y) < 0;
	});
var $ianmackenzie$elm_geometry$Quantity$Extra$rSinTheta = F2(
	function (r, theta) {
		return A2(
			$ianmackenzie$elm_units$Quantity$multiplyBy,
			$ianmackenzie$elm_units$Angle$sin(theta),
			r);
	});
var $ianmackenzie$elm_units$Angle$tan = function (_v0) {
	var angle = _v0.a;
	return $elm$core$Basics$tan(angle);
};
var $ianmackenzie$elm_geometry$Arc2d$pointOn = F2(
	function (_v0, parameterValue) {
		var arc = _v0.a;
		var y0 = $ianmackenzie$elm_geometry$Point2d$yCoordinate(arc.startPoint);
		var x0 = $ianmackenzie$elm_geometry$Point2d$xCoordinate(arc.startPoint);
		var dy = $ianmackenzie$elm_geometry$Direction2d$yComponent(arc.xDirection);
		var dx = $ianmackenzie$elm_geometry$Direction2d$xComponent(arc.xDirection);
		var arcSweptAngle = arc.sweptAngle;
		var arcSignedLength = arc.signedLength;
		if (_Utils_eq(arcSweptAngle, $ianmackenzie$elm_units$Quantity$zero)) {
			var distance = A2($ianmackenzie$elm_units$Quantity$multiplyBy, parameterValue, arcSignedLength);
			var px = A2(
				$ianmackenzie$elm_units$Quantity$plus,
				A2($ianmackenzie$elm_units$Quantity$multiplyBy, dx, distance),
				x0);
			var py = A2(
				$ianmackenzie$elm_units$Quantity$plus,
				A2($ianmackenzie$elm_units$Quantity$multiplyBy, dy, distance),
				y0);
			return A2($ianmackenzie$elm_geometry$Point2d$xy, px, py);
		} else {
			var theta = A2($ianmackenzie$elm_units$Quantity$multiplyBy, parameterValue, arcSweptAngle);
			var arcRadius = A2($ianmackenzie$elm_geometry$Quantity$Extra$lOverTheta, arcSignedLength, arcSweptAngle);
			var x = A2($ianmackenzie$elm_geometry$Quantity$Extra$rSinTheta, arcRadius, theta);
			var y = A2(
				$ianmackenzie$elm_units$Quantity$lessThan,
				$ianmackenzie$elm_units$Angle$radians($elm$core$Basics$pi / 2),
				$ianmackenzie$elm_units$Quantity$abs(theta)) ? A2(
				$ianmackenzie$elm_units$Quantity$multiplyBy,
				$ianmackenzie$elm_units$Angle$tan(
					A2($ianmackenzie$elm_units$Quantity$multiplyBy, 0.5, theta)),
				x) : A2(
				$ianmackenzie$elm_units$Quantity$multiplyBy,
				1 - $ianmackenzie$elm_units$Angle$cos(theta),
				arcRadius);
			var px = A2(
				$ianmackenzie$elm_units$Quantity$plus,
				A4($ianmackenzie$elm_geometry$Quantity$Extra$aXbY, dx, x, -dy, y),
				x0);
			var py = A2(
				$ianmackenzie$elm_units$Quantity$plus,
				A4($ianmackenzie$elm_geometry$Quantity$Extra$aXbY, dy, x, dx, y),
				y0);
			return A2($ianmackenzie$elm_geometry$Point2d$xy, px, py);
		}
	});
var $ianmackenzie$elm_1d_parameter$Parameter1d$range = F5(
	function (startIndex, index, divisor, _function, accumulated) {
		range:
		while (true) {
			var newValue = _function(index / divisor);
			var newAccumulated = A2($elm$core$List$cons, newValue, accumulated);
			if (_Utils_eq(index, startIndex)) {
				return newAccumulated;
			} else {
				var $temp$startIndex = startIndex,
					$temp$index = index - 1,
					$temp$divisor = divisor,
					$temp$function = _function,
					$temp$accumulated = newAccumulated;
				startIndex = $temp$startIndex;
				index = $temp$index;
				divisor = $temp$divisor;
				_function = $temp$function;
				accumulated = $temp$accumulated;
				continue range;
			}
		}
	});
var $ianmackenzie$elm_1d_parameter$Parameter1d$steps = F2(
	function (n, _function) {
		return (n < 1) ? _List_Nil : A5($ianmackenzie$elm_1d_parameter$Parameter1d$range, 0, n, n, _function, _List_Nil);
	});
var $ianmackenzie$elm_geometry$Arc2d$segments = F2(
	function (numSegments, arc) {
		return $ianmackenzie$elm_geometry$Polyline2d$fromVertices(
			A2(
				$ianmackenzie$elm_1d_parameter$Parameter1d$steps,
				numSegments,
				$ianmackenzie$elm_geometry$Arc2d$pointOn(arc)));
	});
var $ianmackenzie$elm_geometry$Polyline2d$vertices = function (_v0) {
	var polylineVertices = _v0.a;
	return polylineVertices;
};
var $author$project$ModelSystem$Level$simulation = F4(
	function (xs, rs, us, model) {
		var yini = 100.0;
		var xini = 100.0;
		var waterFall = 50.0;
		var viewBox = A2(
			$ianmackenzie$elm_geometry$Rectangle2d$from,
			$ianmackenzie$elm_geometry$Point2d$origin,
			A2($ianmackenzie$elm_geometry$Point2d$pixels, 800, 450));
		var reference = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$head(rs));
		var pa = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini, yini);
		var pra22 = pa;
		var prect1 = pa;
		var nSegs = 15;
		var levelmax = 20.0;
		var level = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$head(xs));
		var lbase = 200.0;
		var input = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$head(us));
		var input2 = A2($elm$core$Basics$max, input, 0.0);
		var hbase = 300.0;
		var h = hbase;
		var levelPixelAux = (level / levelmax) * h;
		var levelPixel = A2($elm$core$Basics$min, levelPixelAux, h);
		var pc = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini, yini + h);
		var esc = 30.0;
		var e = 10.0;
		var p3 = A3($author$project$ModelSystem$Level$extendP2d, pa, -e, -e);
		var p8 = A3($author$project$ModelSystem$Level$extendP2d, pc, (-e) - esc, 0.0);
		var p9 = A3($author$project$ModelSystem$Level$extendP2d, p8, 0.0, e);
		var ap = function ($) {
			return $.ap;
		}(model);
		var qo = input2 / ap;
		var angleb = $ianmackenzie$elm_units$Angle$degrees(90.0);
		var anglea = $ianmackenzie$elm_units$Angle$degrees(90.0);
		var ag = function ($) {
			return $.ag;
		}(model);
		var l = lbase * $elm$core$Basics$sqrt(ag);
		var pb = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini + l, yini);
		var p1 = A3($author$project$ModelSystem$Level$extendP2d, pb, esc + e, 0.0);
		var p2 = A3($author$project$ModelSystem$Level$extendP2d, p1, 0.0, -e);
		var prc1 = p1;
		var prb1 = pb;
		var pd = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini + l, yini + h);
		var p10 = A3($author$project$ModelSystem$Level$extendP2d, pd, e, e);
		var prect2 = A3($author$project$ModelSystem$Level$extendP2d, prect1, l, levelPixel);
		var rect = A2($ianmackenzie$elm_geometry$Rectangle2d$from, prect1, prect2);
		var abbase = 20.0;
		var ab = (3 * abbase) * $elm$core$Basics$sqrt(ap);
		var hqo = (_Utils_cmp(qo, ab) > 0) ? ab : ((qo < 0.0) ? 0.0 : qo);
		var hrb = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$minimum(
				_List_fromArray(
					[levelPixel, ab])));
		var parcb1 = A3($author$project$ModelSystem$Level$extendP2d, p1, 0.0, hrb);
		var parcb2 = A3($author$project$ModelSystem$Level$extendP2d, p1, hrb, 0.0);
		var arcb = A3($ianmackenzie$elm_geometry$Arc2d$from, parcb2, parcb1, angleb);
		var arcAsPolylineSegsB = A2($ianmackenzie$elm_geometry$Arc2d$segments, nSegs, arcb);
		var polyb = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			A2(
				$elm$core$List$cons,
				$ianmackenzie$elm_geometry$Arc2d$centerPoint(arcb),
				$ianmackenzie$elm_geometry$Polyline2d$vertices(arcAsPolylineSegsB)));
		var prb2 = A3($author$project$ModelSystem$Level$extendP2d, prb1, e + esc, hrb);
		var rectb = A2($ianmackenzie$elm_geometry$Rectangle2d$from, prb1, prb2);
		var prc2 = A3($author$project$ModelSystem$Level$extendP2d, prc1, hrb, -waterFall);
		var rectBB = A2($ianmackenzie$elm_geometry$Rectangle2d$from, prc2, p9);
		var boundingBox = $ianmackenzie$elm_geometry$Rectangle2d$boundingBox(rectBB);
		var rectBB2 = $ianmackenzie$elm_geometry$Rectangle2d$fromBoundingBox(boundingBox);
		var rectc = A2($ianmackenzie$elm_geometry$Rectangle2d$from, prc1, prc2);
		var p11 = A3($author$project$ModelSystem$Level$extendP2d, pb, e, ab + e);
		var p12 = A3($author$project$ModelSystem$Level$extendP2d, p11, esc, 0.0);
		var p13 = A3($author$project$ModelSystem$Level$extendP2d, p12, 0.0, -e);
		var p14 = A3($author$project$ModelSystem$Level$extendP2d, pb, 0.0, ab);
		var poly2 = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			_List_fromArray(
				[pc, p8, p9, p10, p11, p12, p13, p14, pd]));
		var p4 = A3($author$project$ModelSystem$Level$extendP2d, pc, -e, (-e) - ab);
		var p5 = A3($author$project$ModelSystem$Level$extendP2d, p4, -esc, 0.0);
		var p6 = A3($author$project$ModelSystem$Level$extendP2d, p5, 0.0, e);
		var p7 = A3($author$project$ModelSystem$Level$extendP2d, p6, e + esc, 0.0);
		var pra11 = p6;
		var pra12 = A3($author$project$ModelSystem$Level$extendP2d, pra11, esc + e, hqo);
		var parca1 = pra12;
		var pra21 = A3($author$project$ModelSystem$Level$extendP2d, pra11, (esc + e) + hqo, 0.0);
		var parca2 = pra21;
		var arca = A3($ianmackenzie$elm_geometry$Arc2d$from, parca2, parca1, anglea);
		var arcAsPolylineSegsA = A2($ianmackenzie$elm_geometry$Arc2d$segments, nSegs, arca);
		var polya = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			A2(
				$elm$core$List$cons,
				$ianmackenzie$elm_geometry$Arc2d$centerPoint(arca),
				$ianmackenzie$elm_geometry$Polyline2d$vertices(arcAsPolylineSegsA)));
		var recta2 = A2($ianmackenzie$elm_geometry$Rectangle2d$from, pra21, pra22);
		var recta1 = A2($ianmackenzie$elm_geometry$Rectangle2d$from, pra11, pra12);
		var points = _List_fromArray(
			[pa, pb, pc, pd, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, parca1, parca2, prc1, prc2, parcb1, parcb2]);
		var poly1 = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			_List_fromArray(
				[pa, p1, p2, p3, p4, p5, p6, p7]));
		return $author$project$Drawing2d$draw(
			{
				entities: _List_fromArray(
					[
						A2(
						$author$project$Drawing2d$group,
						_List_fromArray(
							[
								$author$project$Drawing2d$fillColor($avh4$elm_color$Color$lightBlue),
								$author$project$Drawing2d$strokeWidth(
								$ianmackenzie$elm_units$Pixels$float(0.0))
							]),
						_List_fromArray(
							[
								A2($author$project$Drawing2d$rectangle, _List_Nil, rect),
								A2($author$project$Drawing2d$rectangle, _List_Nil, rectb),
								A2($author$project$Drawing2d$rectangle, _List_Nil, recta1),
								A2($author$project$Drawing2d$rectangle, _List_Nil, recta2),
								A2($author$project$Drawing2d$rectangle, _List_Nil, rectc),
								A2($author$project$Drawing2d$polygon, _List_Nil, polya),
								A2($author$project$Drawing2d$polygon, _List_Nil, polyb)
							])),
						A2(
						$author$project$Drawing2d$group,
						_List_fromArray(
							[
								$author$project$Drawing2d$fillColor($avh4$elm_color$Color$grey)
							]),
						_List_fromArray(
							[
								A2($author$project$Drawing2d$polygon, _List_Nil, poly1),
								A2($author$project$Drawing2d$polygon, _List_Nil, poly2)
							]))
					]),
				viewBox: rectBB2
			});
	});
var $author$project$ModelSystem$Level2$extendP2d = F3(
	function (p, dx, dy) {
		var py = $ianmackenzie$elm_units$Pixels$toFloat(
			$ianmackenzie$elm_geometry$Point2d$yCoordinate(p));
		var px = $ianmackenzie$elm_units$Pixels$toFloat(
			$ianmackenzie$elm_geometry$Point2d$xCoordinate(p));
		return A2($ianmackenzie$elm_geometry$Point2d$pixels, px + dx, py + dy);
	});
var $ianmackenzie$elm_geometry$Point2d$toPixels = function (_v0) {
	var pointCoordinates = _v0.a;
	return pointCoordinates;
};
var $author$project$ModelSystem$Level2$simulation = F4(
	function (xs, rs, us, model) {
		var zangleb = $ianmackenzie$elm_units$Angle$degrees(90.0);
		var yini = 100.0;
		var xini = 100.0;
		var waterFall = 350.0;
		var reference = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$head(rs));
		var pa = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini, yini);
		var pra22 = pa;
		var prect1 = pa;
		var newWaterFall = 50;
		var newOrigin = A2($ianmackenzie$elm_geometry$Point2d$pixels, 0, -400);
		var viewBox = A2(
			$ianmackenzie$elm_geometry$Rectangle2d$from,
			newOrigin,
			A2($ianmackenzie$elm_geometry$Point2d$pixels, 800, 450));
		var nSegs = 15;
		var levelmax = 20.0;
		var lbase = 200.0;
		var input = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$head(us));
		var input2 = A2($elm$core$Basics$max, input, 0.0);
		var hbase = 300.0;
		var h = hbase;
		var pc = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini, yini + h);
		var esc = 30.0;
		var e = 10.0;
		var p3 = A3($author$project$ModelSystem$Level2$extendP2d, pa, -e, -e);
		var p8 = A3($author$project$ModelSystem$Level2$extendP2d, pc, (-e) - esc, 0.0);
		var p9 = A3($author$project$ModelSystem$Level2$extendP2d, p8, 0.0, e);
		var ap2 = function ($) {
			return $.ap2;
		}(model);
		var ap1 = function ($) {
			return $.ap1;
		}(model);
		var qo1 = input2 / ap1;
		var angleb = $ianmackenzie$elm_units$Angle$degrees(90.0);
		var anglea = $ianmackenzie$elm_units$Angle$degrees(90.0);
		var ag2 = function ($) {
			return $.ag2;
		}(model);
		var l2 = lbase * $elm$core$Basics$sqrt(ag2);
		var ag1 = function ($) {
			return $.ag1;
		}(model);
		var l1 = lbase * $elm$core$Basics$sqrt(ag1);
		var pb = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini + l1, yini);
		var p1 = A3($author$project$ModelSystem$Level2$extendP2d, pb, esc + e, 0.0);
		var p2 = A3($author$project$ModelSystem$Level2$extendP2d, p1, 0.0, -e);
		var prc1 = p1;
		var zpixels = $ianmackenzie$elm_geometry$Point2d$toPixels(p1);
		var xini2 = zpixels.x - 30;
		var yini2 = zpixels.y - 40;
		var za = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini2, yini2 - h);
		var z3 = A3($author$project$ModelSystem$Level2$extendP2d, za, -e, -e);
		var zrect1 = za;
		var zb = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini2 + l2, yini2 - h);
		var z1 = A3($author$project$ModelSystem$Level2$extendP2d, zb, esc + e, 0.0);
		var z2 = A3($author$project$ModelSystem$Level2$extendP2d, z1, 0.0, -e);
		var zrc1 = z1;
		var zrb1 = zb;
		var zc = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini2, yini2);
		var z4 = A3($author$project$ModelSystem$Level2$extendP2d, zc, -e, e);
		var z7 = A3($author$project$ModelSystem$Level2$extendP2d, z4, e, 0.0);
		var zoly1 = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			_List_fromArray(
				[za, z1, z2, z3, z4, z7]));
		var zd = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini2 + l2, yini2);
		var z10 = A3($author$project$ModelSystem$Level2$extendP2d, zd, e, e);
		var z9 = A3($author$project$ModelSystem$Level2$extendP2d, zd, 0.0, e);
		var prb1 = pb;
		var pd = A2($ianmackenzie$elm_geometry$Point2d$pixels, xini + l1, yini + h);
		var p10 = A3($author$project$ModelSystem$Level2$extendP2d, pd, e, e);
		var abbase = 20.0;
		var ab2 = (3 * abbase) * $elm$core$Basics$sqrt(ap2);
		var z11 = A3($author$project$ModelSystem$Level2$extendP2d, zb, e, ab2 + e);
		var z12 = A3($author$project$ModelSystem$Level2$extendP2d, z11, esc, 0.0);
		var z13 = A3($author$project$ModelSystem$Level2$extendP2d, z12, 0.0, -e);
		var z14 = A3($author$project$ModelSystem$Level2$extendP2d, zb, 0.0, ab2);
		var zoly2 = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			_List_fromArray(
				[z9, z10, z11, z12, z13, z14, z9]));
		var ab1 = (3 * abbase) * $elm$core$Basics$sqrt(ap1);
		var hqo1 = (_Utils_cmp(qo1, ab1) > 0) ? ab1 : ((qo1 < 0.0) ? 0.0 : qo1);
		var p11 = A3($author$project$ModelSystem$Level2$extendP2d, pb, e, ab1 + e);
		var p12 = A3($author$project$ModelSystem$Level2$extendP2d, p11, esc, 0.0);
		var p13 = A3($author$project$ModelSystem$Level2$extendP2d, p12, 0.0, -e);
		var p14 = A3($author$project$ModelSystem$Level2$extendP2d, pb, 0.0, ab1);
		var poly2 = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			_List_fromArray(
				[pc, p8, p9, p10, p11, p12, p13, p14, pd]));
		var p4 = A3($author$project$ModelSystem$Level2$extendP2d, pc, -e, (-e) - ab1);
		var p5 = A3($author$project$ModelSystem$Level2$extendP2d, p4, -esc, 0.0);
		var p6 = A3($author$project$ModelSystem$Level2$extendP2d, p5, 0.0, e);
		var p7 = A3($author$project$ModelSystem$Level2$extendP2d, p6, e + esc, 0.0);
		var pra11 = p6;
		var pra12 = A3($author$project$ModelSystem$Level2$extendP2d, pra11, esc + e, hqo1);
		var parca1 = pra12;
		var pra21 = A3($author$project$ModelSystem$Level2$extendP2d, pra11, (esc + e) + hqo1, 0.0);
		var parca2 = pra21;
		var arca = A3($ianmackenzie$elm_geometry$Arc2d$from, parca2, parca1, anglea);
		var arcAsPolylineSegsA = A2($ianmackenzie$elm_geometry$Arc2d$segments, nSegs, arca);
		var polya = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			A2(
				$elm$core$List$cons,
				$ianmackenzie$elm_geometry$Arc2d$centerPoint(arca),
				$ianmackenzie$elm_geometry$Polyline2d$vertices(arcAsPolylineSegsA)));
		var recta2 = A2($ianmackenzie$elm_geometry$Rectangle2d$from, pra21, pra22);
		var recta1 = A2($ianmackenzie$elm_geometry$Rectangle2d$from, pra11, pra12);
		var poly1 = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			_List_fromArray(
				[pa, p1, p2, p3, p4, p5, p6, p7]));
		var _v0 = function () {
			if (xs.b && xs.b.b) {
				var x1 = xs.a;
				var _v2 = xs.b;
				var x2 = _v2.a;
				var ls = _v2.b;
				return _Utils_Tuple2(x1, x2);
			} else {
				return _Utils_Tuple2(0.0, 0.0);
			}
		}();
		var level1 = _v0.a;
		var level2 = _v0.b;
		var level1PixelAux = (level1 / levelmax) * h;
		var level1Pixel = A2($elm$core$Basics$min, level1PixelAux, h);
		var hrb1 = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$minimum(
				_List_fromArray(
					[level1Pixel, ab1])));
		var parcb1 = A3($author$project$ModelSystem$Level2$extendP2d, p1, 0.0, hrb1);
		var parcb2 = A3($author$project$ModelSystem$Level2$extendP2d, p1, hrb1, 0.0);
		var arcb = A3($ianmackenzie$elm_geometry$Arc2d$from, parcb2, parcb1, angleb);
		var arcAsPolylineSegsB = A2($ianmackenzie$elm_geometry$Arc2d$segments, nSegs, arcb);
		var polyb = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			A2(
				$elm$core$List$cons,
				$ianmackenzie$elm_geometry$Arc2d$centerPoint(arcb),
				$ianmackenzie$elm_geometry$Polyline2d$vertices(arcAsPolylineSegsB)));
		var prb2 = A3($author$project$ModelSystem$Level2$extendP2d, prb1, e + esc, hrb1);
		var rectb = A2($ianmackenzie$elm_geometry$Rectangle2d$from, prb1, prb2);
		var prc2 = A3($author$project$ModelSystem$Level2$extendP2d, prc1, hrb1, -waterFall);
		var points = _List_fromArray(
			[pa, pb, pc, pd, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, parca1, parca2, prc1, prc2, parcb1, parcb2]);
		var rectBB = A2($ianmackenzie$elm_geometry$Rectangle2d$from, prc2, p9);
		var boundingBox = $ianmackenzie$elm_geometry$Rectangle2d$boundingBox(rectBB);
		var rectBB2 = $ianmackenzie$elm_geometry$Rectangle2d$fromBoundingBox(boundingBox);
		var rectc = A2($ianmackenzie$elm_geometry$Rectangle2d$from, prc1, prc2);
		var prect2 = A3($author$project$ModelSystem$Level2$extendP2d, prect1, l1, level1Pixel);
		var rect = A2($ianmackenzie$elm_geometry$Rectangle2d$from, prect1, prect2);
		var level2PixelAux = (level2 / levelmax) * h;
		var level2Pixel = A2($elm$core$Basics$min, level2PixelAux, h);
		var zhrb1 = A2(
			$elm$core$Maybe$withDefault,
			0.0,
			$elm$core$List$minimum(
				_List_fromArray(
					[level2Pixel, ab2])));
		var zarcb1 = A3($author$project$ModelSystem$Level2$extendP2d, z1, 0.0, zhrb1);
		var zarcb2 = A3($author$project$ModelSystem$Level2$extendP2d, z1, zhrb1, 0.0);
		var zarcb = A3($ianmackenzie$elm_geometry$Arc2d$from, zarcb2, zarcb1, zangleb);
		var zarcAsPolylineSegsB = A2($ianmackenzie$elm_geometry$Arc2d$segments, nSegs, zarcb);
		var zolyb = $ianmackenzie$elm_geometry$Polygon2d$singleLoop(
			A2(
				$elm$core$List$cons,
				$ianmackenzie$elm_geometry$Arc2d$centerPoint(zarcb),
				$ianmackenzie$elm_geometry$Polyline2d$vertices(zarcAsPolylineSegsB)));
		var zrb2 = A3($author$project$ModelSystem$Level2$extendP2d, zrb1, e + esc, zhrb1);
		var points2 = _List_fromArray(
			[za, zb, zc, zd, z1, z2, z3, z4, z7, z9, z10, z11, z12, z13, z14, zrb1, zrb2]);
		var zrectb = A2($ianmackenzie$elm_geometry$Rectangle2d$from, zrb1, zrb2);
		var zrc2 = A3($author$project$ModelSystem$Level2$extendP2d, zrc1, zhrb1, -newWaterFall);
		var zrectBB = A2($ianmackenzie$elm_geometry$Rectangle2d$from, zrc2, p9);
		var zboundingBox = $ianmackenzie$elm_geometry$Rectangle2d$boundingBox(zrectBB);
		var zrectBB2 = $ianmackenzie$elm_geometry$Rectangle2d$fromBoundingBox(zboundingBox);
		var zrectc = A2($ianmackenzie$elm_geometry$Rectangle2d$from, zrc1, zrc2);
		var zrect2 = A3($author$project$ModelSystem$Level2$extendP2d, zrect1, l2, level2Pixel);
		var zrect = A2($ianmackenzie$elm_geometry$Rectangle2d$from, zrect1, zrect2);
		return $author$project$Drawing2d$draw(
			{
				entities: _List_fromArray(
					[
						A2(
						$author$project$Drawing2d$group,
						_List_fromArray(
							[
								$author$project$Drawing2d$fillColor($avh4$elm_color$Color$lightBlue),
								$author$project$Drawing2d$strokeWidth(
								$ianmackenzie$elm_units$Pixels$float(0.0))
							]),
						_List_fromArray(
							[
								A2($author$project$Drawing2d$rectangle, _List_Nil, rect),
								A2($author$project$Drawing2d$rectangle, _List_Nil, rectb),
								A2($author$project$Drawing2d$rectangle, _List_Nil, recta1),
								A2($author$project$Drawing2d$rectangle, _List_Nil, recta2),
								A2($author$project$Drawing2d$rectangle, _List_Nil, rectc),
								A2($author$project$Drawing2d$polygon, _List_Nil, polya),
								A2($author$project$Drawing2d$polygon, _List_Nil, polyb)
							])),
						A2(
						$author$project$Drawing2d$group,
						_List_fromArray(
							[
								$author$project$Drawing2d$fillColor($avh4$elm_color$Color$grey)
							]),
						_List_fromArray(
							[
								A2($author$project$Drawing2d$polygon, _List_Nil, poly1),
								A2($author$project$Drawing2d$polygon, _List_Nil, poly2)
							])),
						A2(
						$author$project$Drawing2d$group,
						_List_fromArray(
							[
								$author$project$Drawing2d$fillColor($avh4$elm_color$Color$lightBlue),
								$author$project$Drawing2d$strokeWidth(
								$ianmackenzie$elm_units$Pixels$float(0.0))
							]),
						_List_fromArray(
							[
								A2($author$project$Drawing2d$rectangle, _List_Nil, zrect),
								A2($author$project$Drawing2d$rectangle, _List_Nil, zrectb),
								A2($author$project$Drawing2d$rectangle, _List_Nil, zrectc),
								A2($author$project$Drawing2d$polygon, _List_Nil, zolyb)
							])),
						A2(
						$author$project$Drawing2d$group,
						_List_fromArray(
							[
								$author$project$Drawing2d$fillColor($avh4$elm_color$Color$grey)
							]),
						_List_fromArray(
							[
								A2($author$project$Drawing2d$polygon, _List_Nil, zoly1),
								A2($author$project$Drawing2d$polygon, _List_Nil, zoly2)
							]))
					]),
				viewBox: zrectBB2
			});
	});
var $author$project$ModelSystem$simulation = F4(
	function (xs, rs, us, model) {
		if (model.$ === 'LevelModel') {
			var levelModel = model.a;
			return A4($author$project$ModelSystem$Level$simulation, xs, rs, us, levelModel);
		} else {
			var level2Model = model.a;
			return A4($author$project$ModelSystem$Level2$simulation, xs, rs, us, level2Model);
		}
	});
var $author$project$DataConvert$usFromDatum = function (chartDatum) {
	switch (chartDatum.$) {
		case 'TS1':
			var datum = chartDatum.a;
			return _List_Nil;
		case 'TS1E1R1U1':
			var datum = chartDatum.a;
			return _List_fromArray(
				[
					function ($) {
					return $.u1;
				}(datum)
				]);
		case 'TS1E1R1U4':
			var datum = chartDatum.a;
			return _List_fromArray(
				[
					function ($) {
					return $.u1;
				}(datum),
					function ($) {
					return $.u2;
				}(datum),
					function ($) {
					return $.u3;
				}(datum),
					function ($) {
					return $.u4;
				}(datum)
				]);
		default:
			var datum = chartDatum.a;
			return _List_fromArray(
				[
					function ($) {
					return $.u1;
				}(datum),
					function ($) {
					return $.u2;
				}(datum),
					function ($) {
					return $.u3;
				}(datum),
					function ($) {
					return $.u4;
				}(datum)
				]);
	}
};
var $author$project$Controller$PidMsg = function (a) {
	return {$: 'PidMsg', a: a};
};
var $author$project$Controller$PideqMsg = function (a) {
	return {$: 'PideqMsg', a: a};
};
var $author$project$Controller$PID$PidKd = function (a) {
	return {$: 'PidKd', a: a};
};
var $author$project$Controller$PID$PidKi = function (a) {
	return {$: 'PidKi', a: a};
};
var $author$project$Controller$PID$PidKp = function (a) {
	return {$: 'PidKp', a: a};
};
var $author$project$UI$heading = function (str) {
	return A2(
		$mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$centerX,
				$mdgriffith$elm_ui$Element$alignTop,
				$mdgriffith$elm_ui$Element$Font$size(20),
				$mdgriffith$elm_ui$Element$Font$variant($mdgriffith$elm_ui$Element$Font$smallCaps)
			]),
		$mdgriffith$elm_ui$Element$text(str));
};
var $mdgriffith$elm_ui$Element$htmlAttribute = $mdgriffith$elm_ui$Internal$Model$Attr;
var $mdgriffith$elm_ui$Element$Input$Label = F3(
	function (a, b, c) {
		return {$: 'Label', a: a, b: b, c: c};
	});
var $mdgriffith$elm_ui$Element$Input$OnLeft = {$: 'OnLeft'};
var $mdgriffith$elm_ui$Element$Input$labelLeft = $mdgriffith$elm_ui$Element$Input$Label($mdgriffith$elm_ui$Element$Input$OnLeft);
var $mdgriffith$elm_ui$Internal$Model$paddingName = F4(
	function (top, right, bottom, left) {
		return 'pad-' + ($elm$core$String$fromInt(top) + ('-' + ($elm$core$String$fromInt(right) + ('-' + ($elm$core$String$fromInt(bottom) + ('-' + $elm$core$String$fromInt(left)))))));
	});
var $mdgriffith$elm_ui$Element$paddingEach = function (_v0) {
	var top = _v0.top;
	var right = _v0.right;
	var bottom = _v0.bottom;
	var left = _v0.left;
	if (_Utils_eq(top, right) && (_Utils_eq(top, bottom) && _Utils_eq(top, left))) {
		var topFloat = top;
		return A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + $elm$core$String$fromInt(top),
				topFloat,
				topFloat,
				topFloat,
				topFloat));
	} else {
		return A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				A4($mdgriffith$elm_ui$Internal$Model$paddingName, top, right, bottom, left),
				top,
				right,
				bottom,
				left));
	}
};
var $mdgriffith$elm_ui$Element$Input$TextInputNode = function (a) {
	return {$: 'TextInputNode', a: a};
};
var $mdgriffith$elm_ui$Element$Input$TextArea = {$: 'TextArea'};
var $mdgriffith$elm_ui$Internal$Model$LivePolite = {$: 'LivePolite'};
var $mdgriffith$elm_ui$Element$Region$announce = $mdgriffith$elm_ui$Internal$Model$Describe($mdgriffith$elm_ui$Internal$Model$LivePolite);
var $mdgriffith$elm_ui$Element$Input$applyLabel = F3(
	function (attrs, label, input) {
		if (label.$ === 'HiddenLabel') {
			var labelText = label.a;
			return A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asColumn,
				$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
				attrs,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[input])));
		} else {
			var position = label.a;
			var labelAttrs = label.b;
			var labelChild = label.c;
			var labelElement = A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				labelAttrs,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[labelChild])));
			switch (position.$) {
				case 'Above':
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputLabel),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
				case 'Below':
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asColumn,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputLabel),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				case 'OnRight':
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputLabel),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				default:
					return A4(
						$mdgriffith$elm_ui$Internal$Model$element,
						$mdgriffith$elm_ui$Internal$Model$asRow,
						$mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						A2(
							$elm$core$List$cons,
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputLabel),
							attrs),
						$mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
			}
		}
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $mdgriffith$elm_ui$Element$Input$autofill = A2(
	$elm$core$Basics$composeL,
	$mdgriffith$elm_ui$Internal$Model$Attr,
	$elm$html$Html$Attributes$attribute('autocomplete'));
var $mdgriffith$elm_ui$Internal$Model$Behind = {$: 'Behind'};
var $mdgriffith$elm_ui$Internal$Model$Nearby = F2(
	function (a, b) {
		return {$: 'Nearby', a: a, b: b};
	});
var $mdgriffith$elm_ui$Element$createNearby = F2(
	function (loc, element) {
		if (element.$ === 'Empty') {
			return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
		} else {
			return A2($mdgriffith$elm_ui$Internal$Model$Nearby, loc, element);
		}
	});
var $mdgriffith$elm_ui$Element$behindContent = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, $mdgriffith$elm_ui$Internal$Model$Behind, element);
};
var $mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding = function (attrs) {
	var gatherSpacing = F2(
		function (attr, found) {
			if ((attr.$ === 'StyleClass') && (attr.b.$ === 'SpacingStyle')) {
				var _v2 = attr.b;
				var x = _v2.b;
				var y = _v2.c;
				if (found.$ === 'Nothing') {
					return $elm$core$Maybe$Just(y);
				} else {
					return found;
				}
			} else {
				return found;
			}
		});
	var _v0 = A3($elm$core$List$foldr, gatherSpacing, $elm$core$Maybe$Nothing, attrs);
	if (_v0.$ === 'Nothing') {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	} else {
		var vSpace = _v0.a;
		return $mdgriffith$elm_ui$Element$moveUp(
			$elm$core$Basics$floor(vSpace / 2));
	}
};
var $mdgriffith$elm_ui$Internal$Flag$overflow = $mdgriffith$elm_ui$Internal$Flag$flag(20);
var $mdgriffith$elm_ui$Element$clip = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.clip);
var $mdgriffith$elm_ui$Internal$Flag$bgColor = $mdgriffith$elm_ui$Internal$Flag$flag(8);
var $mdgriffith$elm_ui$Element$Background$color = function (clr) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$bgColor,
		A3(
			$mdgriffith$elm_ui$Internal$Model$Colored,
			'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'background-color',
			clr));
};
var $mdgriffith$elm_ui$Element$rgb = F3(
	function (r, g, b) {
		return A4($mdgriffith$elm_ui$Internal$Model$Rgba, r, g, b, 1);
	});
var $mdgriffith$elm_ui$Element$Input$darkGrey = A3($mdgriffith$elm_ui$Element$rgb, 186 / 255, 189 / 255, 182 / 255);
var $mdgriffith$elm_ui$Element$paddingXY = F2(
	function (x, y) {
		if (_Utils_eq(x, y)) {
			var f = x;
			return A2(
				$mdgriffith$elm_ui$Internal$Model$StyleClass,
				$mdgriffith$elm_ui$Internal$Flag$padding,
				A5(
					$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
					'p-' + $elm$core$String$fromInt(x),
					f,
					f,
					f,
					f));
		} else {
			var yFloat = y;
			var xFloat = x;
			return A2(
				$mdgriffith$elm_ui$Internal$Model$StyleClass,
				$mdgriffith$elm_ui$Internal$Flag$padding,
				A5(
					$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
					'p-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y))),
					yFloat,
					xFloat,
					yFloat,
					xFloat));
		}
	});
var $mdgriffith$elm_ui$Element$Input$defaultTextPadding = A2($mdgriffith$elm_ui$Element$paddingXY, 12, 12);
var $mdgriffith$elm_ui$Element$Input$white = A3($mdgriffith$elm_ui$Element$rgb, 1, 1, 1);
var $mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle = _List_fromArray(
	[
		$mdgriffith$elm_ui$Element$Input$defaultTextPadding,
		$mdgriffith$elm_ui$Element$Border$rounded(3),
		$mdgriffith$elm_ui$Element$Border$color($mdgriffith$elm_ui$Element$Input$darkGrey),
		$mdgriffith$elm_ui$Element$Background$color($mdgriffith$elm_ui$Element$Input$white),
		$mdgriffith$elm_ui$Element$Border$width(1),
		$mdgriffith$elm_ui$Element$spacing(5),
		$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
		$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$shrink)
	]);
var $mdgriffith$elm_ui$Element$Input$getHeight = function (attr) {
	if (attr.$ === 'Height') {
		var h = attr.a;
		return $elm$core$Maybe$Just(h);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $mdgriffith$elm_ui$Internal$Model$Label = function (a) {
	return {$: 'Label', a: a};
};
var $mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute = function (label) {
	if (label.$ === 'HiddenLabel') {
		var textLabel = label.a;
		return $mdgriffith$elm_ui$Internal$Model$Describe(
			$mdgriffith$elm_ui$Internal$Model$Label(textLabel));
	} else {
		return $mdgriffith$elm_ui$Internal$Model$NoAttribute;
	}
};
var $mdgriffith$elm_ui$Internal$Model$InFront = {$: 'InFront'};
var $mdgriffith$elm_ui$Element$inFront = function (element) {
	return A2($mdgriffith$elm_ui$Element$createNearby, $mdgriffith$elm_ui$Internal$Model$InFront, element);
};
var $mdgriffith$elm_ui$Element$Input$isConstrained = function (len) {
	isConstrained:
	while (true) {
		switch (len.$) {
			case 'Content':
				return false;
			case 'Px':
				return true;
			case 'Fill':
				return true;
			case 'Min':
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isConstrained;
			default:
				var l = len.b;
				return true;
		}
	}
};
var $mdgriffith$elm_ui$Element$Input$isHiddenLabel = function (label) {
	if (label.$ === 'HiddenLabel') {
		return true;
	} else {
		return false;
	}
};
var $mdgriffith$elm_ui$Element$Input$isStacked = function (label) {
	if (label.$ === 'Label') {
		var loc = label.a;
		switch (loc.$) {
			case 'OnRight':
				return false;
			case 'OnLeft':
				return false;
			case 'Above':
				return true;
			default:
				return true;
		}
	} else {
		return true;
	}
};
var $mdgriffith$elm_ui$Element$Input$negateBox = function (box) {
	return {bottom: -box.bottom, left: -box.left, right: -box.right, top: -box.top};
};
var $mdgriffith$elm_ui$Element$Input$isFill = function (len) {
	isFill:
	while (true) {
		switch (len.$) {
			case 'Fill':
				return true;
			case 'Content':
				return false;
			case 'Px':
				return false;
			case 'Min':
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isFill;
			default:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isFill;
		}
	}
};
var $mdgriffith$elm_ui$Element$Input$isPixel = function (len) {
	isPixel:
	while (true) {
		switch (len.$) {
			case 'Content':
				return false;
			case 'Px':
				return true;
			case 'Fill':
				return false;
			case 'Min':
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isPixel;
			default:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isPixel;
		}
	}
};
var $mdgriffith$elm_ui$Internal$Model$paddingNameFloat = F4(
	function (top, right, bottom, left) {
		return 'pad-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(top) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(right) + ('-' + ($mdgriffith$elm_ui$Internal$Model$floatClass(bottom) + ('-' + $mdgriffith$elm_ui$Internal$Model$floatClass(left)))))));
	});
var $mdgriffith$elm_ui$Element$Input$redistributeOver = F4(
	function (isMultiline, stacked, attr, els) {
		switch (attr.$) {
			case 'Nearby':
				return _Utils_update(
					els,
					{
						parent: A2($elm$core$List$cons, attr, els.parent)
					});
			case 'Width':
				var width = attr.a;
				return $mdgriffith$elm_ui$Element$Input$isFill(width) ? _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent),
						input: A2($elm$core$List$cons, attr, els.input),
						parent: A2($elm$core$List$cons, attr, els.parent)
					}) : (stacked ? _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent)
					}) : _Utils_update(
					els,
					{
						parent: A2($elm$core$List$cons, attr, els.parent)
					}));
			case 'Height':
				var height = attr.a;
				return (!stacked) ? _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent),
						parent: A2($elm$core$List$cons, attr, els.parent)
					}) : ($mdgriffith$elm_ui$Element$Input$isFill(height) ? _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent),
						parent: A2($elm$core$List$cons, attr, els.parent)
					}) : ($mdgriffith$elm_ui$Element$Input$isPixel(height) ? _Utils_update(
					els,
					{
						parent: A2($elm$core$List$cons, attr, els.parent)
					}) : _Utils_update(
					els,
					{
						parent: A2($elm$core$List$cons, attr, els.parent)
					})));
			case 'AlignX':
				return _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent)
					});
			case 'AlignY':
				return _Utils_update(
					els,
					{
						fullParent: A2($elm$core$List$cons, attr, els.fullParent)
					});
			case 'StyleClass':
				switch (attr.b.$) {
					case 'SpacingStyle':
						var _v1 = attr.b;
						return _Utils_update(
							els,
							{
								fullParent: A2($elm$core$List$cons, attr, els.fullParent),
								input: A2($elm$core$List$cons, attr, els.input),
								parent: A2($elm$core$List$cons, attr, els.parent),
								wrapper: A2($elm$core$List$cons, attr, els.wrapper)
							});
					case 'PaddingStyle':
						var cls = attr.a;
						var _v2 = attr.b;
						var pad = _v2.a;
						var t = _v2.b;
						var r = _v2.c;
						var b = _v2.d;
						var l = _v2.e;
						if (isMultiline) {
							return _Utils_update(
								els,
								{
									cover: A2($elm$core$List$cons, attr, els.cover),
									parent: A2($elm$core$List$cons, attr, els.parent)
								});
						} else {
							var newTop = t - A2($elm$core$Basics$min, t, b);
							var newLineHeight = $mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Attributes$style,
									'line-height',
									'calc(1.0em + ' + ($elm$core$String$fromFloat(
										2 * A2($elm$core$Basics$min, t, b)) + 'px)')));
							var newHeight = $mdgriffith$elm_ui$Element$htmlAttribute(
								A2(
									$elm$html$Html$Attributes$style,
									'height',
									'calc(1.0em + ' + ($elm$core$String$fromFloat(
										2 * A2($elm$core$Basics$min, t, b)) + 'px)')));
							var newBottom = b - A2($elm$core$Basics$min, t, b);
							var reducedVerticalPadding = A2(
								$mdgriffith$elm_ui$Internal$Model$StyleClass,
								$mdgriffith$elm_ui$Internal$Flag$padding,
								A5(
									$mdgriffith$elm_ui$Internal$Model$PaddingStyle,
									A4($mdgriffith$elm_ui$Internal$Model$paddingNameFloat, newTop, r, newBottom, l),
									newTop,
									r,
									newBottom,
									l));
							return _Utils_update(
								els,
								{
									cover: A2($elm$core$List$cons, attr, els.cover),
									input: A2(
										$elm$core$List$cons,
										newHeight,
										A2($elm$core$List$cons, newLineHeight, els.input)),
									parent: A2($elm$core$List$cons, reducedVerticalPadding, els.parent)
								});
						}
					case 'BorderWidth':
						var _v3 = attr.b;
						return _Utils_update(
							els,
							{
								cover: A2($elm$core$List$cons, attr, els.cover),
								parent: A2($elm$core$List$cons, attr, els.parent)
							});
					case 'Transform':
						return _Utils_update(
							els,
							{
								cover: A2($elm$core$List$cons, attr, els.cover),
								parent: A2($elm$core$List$cons, attr, els.parent)
							});
					case 'FontSize':
						return _Utils_update(
							els,
							{
								fullParent: A2($elm$core$List$cons, attr, els.fullParent)
							});
					case 'FontFamily':
						var _v4 = attr.b;
						return _Utils_update(
							els,
							{
								fullParent: A2($elm$core$List$cons, attr, els.fullParent)
							});
					default:
						var flag = attr.a;
						var cls = attr.b;
						return _Utils_update(
							els,
							{
								parent: A2($elm$core$List$cons, attr, els.parent)
							});
				}
			case 'NoAttribute':
				return els;
			case 'Attr':
				var a = attr.a;
				return _Utils_update(
					els,
					{
						input: A2($elm$core$List$cons, attr, els.input)
					});
			case 'Describe':
				return _Utils_update(
					els,
					{
						input: A2($elm$core$List$cons, attr, els.input)
					});
			case 'Class':
				return _Utils_update(
					els,
					{
						parent: A2($elm$core$List$cons, attr, els.parent)
					});
			default:
				return _Utils_update(
					els,
					{
						input: A2($elm$core$List$cons, attr, els.input)
					});
		}
	});
var $mdgriffith$elm_ui$Element$Input$redistribute = F3(
	function (isMultiline, stacked, attrs) {
		return function (redist) {
			return {
				cover: $elm$core$List$reverse(redist.cover),
				fullParent: $elm$core$List$reverse(redist.fullParent),
				input: $elm$core$List$reverse(redist.input),
				parent: $elm$core$List$reverse(redist.parent),
				wrapper: $elm$core$List$reverse(redist.wrapper)
			};
		}(
			A3(
				$elm$core$List$foldl,
				A2($mdgriffith$elm_ui$Element$Input$redistributeOver, isMultiline, stacked),
				{cover: _List_Nil, fullParent: _List_Nil, input: _List_Nil, parent: _List_Nil, wrapper: _List_Nil},
				attrs));
	});
var $mdgriffith$elm_ui$Element$Input$renderBox = function (_v0) {
	var top = _v0.top;
	var right = _v0.right;
	var bottom = _v0.bottom;
	var left = _v0.left;
	return $elm$core$String$fromInt(top) + ('px ' + ($elm$core$String$fromInt(right) + ('px ' + ($elm$core$String$fromInt(bottom) + ('px ' + ($elm$core$String$fromInt(left) + 'px'))))));
};
var $mdgriffith$elm_ui$Internal$Model$Transparency = F2(
	function (a, b) {
		return {$: 'Transparency', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$transparency = $mdgriffith$elm_ui$Internal$Flag$flag(0);
var $mdgriffith$elm_ui$Element$alpha = function (o) {
	var transparency = function (x) {
		return 1 - x;
	}(
		A2(
			$elm$core$Basics$min,
			1.0,
			A2($elm$core$Basics$max, 0.0, o)));
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$transparency,
		A2(
			$mdgriffith$elm_ui$Internal$Model$Transparency,
			'transparency-' + $mdgriffith$elm_ui$Internal$Model$floatClass(transparency),
			transparency));
};
var $mdgriffith$elm_ui$Element$Input$charcoal = A3($mdgriffith$elm_ui$Element$rgb, 136 / 255, 138 / 255, 133 / 255);
var $mdgriffith$elm_ui$Element$rgba = $mdgriffith$elm_ui$Internal$Model$Rgba;
var $mdgriffith$elm_ui$Element$Input$renderPlaceholder = F3(
	function (_v0, forPlaceholder, on) {
		var placeholderAttrs = _v0.a;
		var placeholderEl = _v0.b;
		return A2(
			$mdgriffith$elm_ui$Element$el,
			_Utils_ap(
				forPlaceholder,
				_Utils_ap(
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$color($mdgriffith$elm_ui$Element$Input$charcoal),
							$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.noTextSelection + (' ' + $mdgriffith$elm_ui$Internal$Style$classes.passPointerEvents)),
							$mdgriffith$elm_ui$Element$clip,
							$mdgriffith$elm_ui$Element$Border$color(
							A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
							$mdgriffith$elm_ui$Element$Background$color(
							A4($mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
							$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
							$mdgriffith$elm_ui$Element$alpha(
							on ? 1 : 0)
						]),
					placeholderAttrs)),
			placeholderEl);
	});
var $mdgriffith$elm_ui$Element$scrollbarY = A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$overflow, $mdgriffith$elm_ui$Internal$Style$classes.scrollbarsY);
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$spellcheck = $elm$html$Html$Attributes$boolProperty('spellcheck');
var $mdgriffith$elm_ui$Element$Input$spellcheck = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$spellcheck);
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $mdgriffith$elm_ui$Element$Input$value = A2($elm$core$Basics$composeL, $mdgriffith$elm_ui$Internal$Model$Attr, $elm$html$Html$Attributes$value);
var $mdgriffith$elm_ui$Element$Input$textHelper = F3(
	function (textInput, attrs, textOptions) {
		var withDefaults = _Utils_ap($mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle, attrs);
		var redistributed = A3(
			$mdgriffith$elm_ui$Element$Input$redistribute,
			_Utils_eq(textInput.type_, $mdgriffith$elm_ui$Element$Input$TextArea),
			$mdgriffith$elm_ui$Element$Input$isStacked(textOptions.label),
			withDefaults);
		var onlySpacing = function (attr) {
			if ((attr.$ === 'StyleClass') && (attr.b.$ === 'SpacingStyle')) {
				var _v9 = attr.b;
				return true;
			} else {
				return false;
			}
		};
		var heightConstrained = function () {
			var _v7 = textInput.type_;
			if (_v7.$ === 'TextInputNode') {
				var inputType = _v7.a;
				return false;
			} else {
				return A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						$mdgriffith$elm_ui$Element$Input$isConstrained,
						$elm$core$List$head(
							$elm$core$List$reverse(
								A2($elm$core$List$filterMap, $mdgriffith$elm_ui$Element$Input$getHeight, withDefaults)))));
			}
		}();
		var getPadding = function (attr) {
			if ((attr.$ === 'StyleClass') && (attr.b.$ === 'PaddingStyle')) {
				var cls = attr.a;
				var _v6 = attr.b;
				var pad = _v6.a;
				var t = _v6.b;
				var r = _v6.c;
				var b = _v6.d;
				var l = _v6.e;
				return $elm$core$Maybe$Just(
					{
						bottom: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(b - 3)),
						left: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(l - 3)),
						right: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(r - 3)),
						top: A2(
							$elm$core$Basics$max,
							0,
							$elm$core$Basics$floor(t - 3))
					});
			} else {
				return $elm$core$Maybe$Nothing;
			}
		};
		var parentPadding = A2(
			$elm$core$Maybe$withDefault,
			{bottom: 0, left: 0, right: 0, top: 0},
			$elm$core$List$head(
				$elm$core$List$reverse(
					A2($elm$core$List$filterMap, getPadding, withDefaults))));
		var inputElement = A4(
			$mdgriffith$elm_ui$Internal$Model$element,
			$mdgriffith$elm_ui$Internal$Model$asEl,
			function () {
				var _v3 = textInput.type_;
				if (_v3.$ === 'TextInputNode') {
					var inputType = _v3.a;
					return $mdgriffith$elm_ui$Internal$Model$NodeName('input');
				} else {
					return $mdgriffith$elm_ui$Internal$Model$NodeName('textarea');
				}
			}(),
			_Utils_ap(
				function () {
					var _v4 = textInput.type_;
					if (_v4.$ === 'TextInputNode') {
						var inputType = _v4.a;
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Internal$Model$Attr(
								$elm$html$Html$Attributes$type_(inputType)),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputText)
							]);
					} else {
						return _List_fromArray(
							[
								$mdgriffith$elm_ui$Element$clip,
								$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
								$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputMultiline),
								$mdgriffith$elm_ui$Element$Input$calcMoveToCompensateForPadding(withDefaults),
								$mdgriffith$elm_ui$Element$paddingEach(parentPadding),
								$mdgriffith$elm_ui$Internal$Model$Attr(
								A2(
									$elm$html$Html$Attributes$style,
									'margin',
									$mdgriffith$elm_ui$Element$Input$renderBox(
										$mdgriffith$elm_ui$Element$Input$negateBox(parentPadding)))),
								$mdgriffith$elm_ui$Internal$Model$Attr(
								A2($elm$html$Html$Attributes$style, 'box-sizing', 'content-box'))
							]);
					}
				}(),
				_Utils_ap(
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Input$value(textOptions.text),
							$mdgriffith$elm_ui$Internal$Model$Attr(
							$elm$html$Html$Events$onInput(textOptions.onChange)),
							$mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute(textOptions.label),
							$mdgriffith$elm_ui$Element$Input$spellcheck(textInput.spellchecked),
							A2(
							$elm$core$Maybe$withDefault,
							$mdgriffith$elm_ui$Internal$Model$NoAttribute,
							A2($elm$core$Maybe$map, $mdgriffith$elm_ui$Element$Input$autofill, textInput.autofill))
						]),
					redistributed.input)),
			$mdgriffith$elm_ui$Internal$Model$Unkeyed(_List_Nil));
		var wrappedInput = function () {
			var _v0 = textInput.type_;
			if (_v0.$ === 'TextArea') {
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					_Utils_ap(
						(heightConstrained ? $elm$core$List$cons($mdgriffith$elm_ui$Element$scrollbarY) : $elm$core$Basics$identity)(
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.focusedWithin),
									$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineWrapper)
								])),
						redistributed.parent),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[
								A4(
								$mdgriffith$elm_ui$Internal$Model$element,
								$mdgriffith$elm_ui$Internal$Model$asParagraph,
								$mdgriffith$elm_ui$Internal$Model$div,
								A2(
									$elm$core$List$cons,
									$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
									A2(
										$elm$core$List$cons,
										$mdgriffith$elm_ui$Element$height($mdgriffith$elm_ui$Element$fill),
										A2(
											$elm$core$List$cons,
											$mdgriffith$elm_ui$Element$inFront(inputElement),
											A2(
												$elm$core$List$cons,
												$mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineParent),
												redistributed.wrapper)))),
								$mdgriffith$elm_ui$Internal$Model$Unkeyed(
									function () {
										if (textOptions.text === '') {
											var _v1 = textOptions.placeholder;
											if (_v1.$ === 'Nothing') {
												return _List_fromArray(
													[
														$mdgriffith$elm_ui$Element$text('\u00A0')
													]);
											} else {
												var place = _v1.a;
												return _List_fromArray(
													[
														A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, _List_Nil, textOptions.text === '')
													]);
											}
										} else {
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Internal$Model$unstyled(
													A2(
														$elm$html$Html$span,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class($mdgriffith$elm_ui$Internal$Style$classes.inputMultilineFiller)
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(textOptions.text + '\u00A0')
															])))
												]);
										}
									}()))
							])));
			} else {
				var inputType = _v0.a;
				return A4(
					$mdgriffith$elm_ui$Internal$Model$element,
					$mdgriffith$elm_ui$Internal$Model$asEl,
					$mdgriffith$elm_ui$Internal$Model$div,
					A2(
						$elm$core$List$cons,
						$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
						A2(
							$elm$core$List$cons,
							A2($elm$core$List$any, $mdgriffith$elm_ui$Element$Input$hasFocusStyle, withDefaults) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Internal$Model$htmlClass($mdgriffith$elm_ui$Internal$Style$classes.focusedWithin),
							$elm$core$List$concat(
								_List_fromArray(
									[
										redistributed.parent,
										function () {
										var _v2 = textOptions.placeholder;
										if (_v2.$ === 'Nothing') {
											return _List_Nil;
										} else {
											var place = _v2.a;
											return _List_fromArray(
												[
													$mdgriffith$elm_ui$Element$behindContent(
													A3($mdgriffith$elm_ui$Element$Input$renderPlaceholder, place, redistributed.cover, textOptions.text === ''))
												]);
										}
									}()
									])))),
					$mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[inputElement])));
			}
		}();
		return A3(
			$mdgriffith$elm_ui$Element$Input$applyLabel,
			A2(
				$elm$core$List$cons,
				A2($mdgriffith$elm_ui$Internal$Model$Class, $mdgriffith$elm_ui$Internal$Flag$cursor, $mdgriffith$elm_ui$Internal$Style$classes.cursorText),
				A2(
					$elm$core$List$cons,
					$mdgriffith$elm_ui$Element$Input$isHiddenLabel(textOptions.label) ? $mdgriffith$elm_ui$Internal$Model$NoAttribute : $mdgriffith$elm_ui$Element$spacing(5),
					A2($elm$core$List$cons, $mdgriffith$elm_ui$Element$Region$announce, redistributed.fullParent))),
			textOptions.label,
			wrappedInput);
	});
var $mdgriffith$elm_ui$Element$Input$text = $mdgriffith$elm_ui$Element$Input$textHelper(
	{
		autofill: $elm$core$Maybe$Nothing,
		spellchecked: false,
		type_: $mdgriffith$elm_ui$Element$Input$TextInputNode('text')
	});
var $mdgriffith$elm_ui$Element$Border$widthXY = F2(
	function (x, y) {
		return A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$borderWidth,
			A5(
				$mdgriffith$elm_ui$Internal$Model$BorderWidth,
				'b-' + ($elm$core$String$fromInt(x) + ('-' + $elm$core$String$fromInt(y))),
				y,
				x,
				y,
				x));
	});
var $mdgriffith$elm_ui$Element$Border$widthEach = function (_v0) {
	var bottom = _v0.bottom;
	var top = _v0.top;
	var left = _v0.left;
	var right = _v0.right;
	return (_Utils_eq(top, bottom) && _Utils_eq(left, right)) ? (_Utils_eq(top, right) ? $mdgriffith$elm_ui$Element$Border$width(top) : A2($mdgriffith$elm_ui$Element$Border$widthXY, left, top)) : A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			$mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + ($elm$core$String$fromInt(top) + ('-' + ($elm$core$String$fromInt(right) + ('-' + ($elm$core$String$fromInt(bottom) + ('-' + $elm$core$String$fromInt(left))))))),
			top,
			right,
			bottom,
			left));
};
var $author$project$UI$textField = F3(
	function (str, label, strToMsg) {
		return A2(
			$mdgriffith$elm_ui$Element$Input$text,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$htmlAttribute(
					$elm$html$Html$Attributes$type_('number')),
					$mdgriffith$elm_ui$Element$Border$widthEach(
					{bottom: 1, left: 0, right: 0, top: 0}),
					$mdgriffith$elm_ui$Element$Border$rounded(0),
					$mdgriffith$elm_ui$Element$width(
					$mdgriffith$elm_ui$Element$px(70)),
					$mdgriffith$elm_ui$Element$paddingEach(
					{bottom: 1, left: 10, right: 0, top: 10}),
					$mdgriffith$elm_ui$Element$spacing(20),
					$mdgriffith$elm_ui$Element$Font$size(18),
					$mdgriffith$elm_ui$Element$Font$color(
					A3($mdgriffith$elm_ui$Element$rgb255, 100, 100, 100)),
					$mdgriffith$elm_ui$Element$Font$center
				]),
			{
				label: A2(
					$mdgriffith$elm_ui$Element$Input$labelLeft,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$Font$size(18),
							$mdgriffith$elm_ui$Element$Font$color(
							A3($mdgriffith$elm_ui$Element$rgb255, 50, 50, 50)),
							$mdgriffith$elm_ui$Element$Font$variant($mdgriffith$elm_ui$Element$Font$smallCaps)
						]),
					$mdgriffith$elm_ui$Element$text(label)),
				onChange: strToMsg,
				placeholder: $elm$core$Maybe$Nothing,
				text: str
			});
	});
var $author$project$Controller$PID$view = F2(
	function (model, msgToMainMsg) {
		var kpStr = function ($) {
			return $.kpStr;
		}(model);
		var kiStr = function ($) {
			return $.kiStr;
		}(model);
		var kdStr = function ($) {
			return $.kdStr;
		}(model);
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$spacing(10),
					$mdgriffith$elm_ui$Element$padding(20),
					$mdgriffith$elm_ui$Element$centerX
				]),
			_List_fromArray(
				[
					$author$project$UI$heading('Controller'),
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$spacing(35)
						]),
					_List_fromArray(
						[
							A3(
							$author$project$UI$textField,
							kpStr,
							'Kp',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Controller$PID$PidKp)),
							A3(
							$author$project$UI$textField,
							kiStr,
							'Ki',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Controller$PID$PidKi)),
							A3(
							$author$project$UI$textField,
							kdStr,
							'Kd',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Controller$PID$PidKd))
						]))
				]));
	});
var $author$project$Controller$PIDeq$PidKd = function (a) {
	return {$: 'PidKd', a: a};
};
var $author$project$Controller$PIDeq$PidKi = function (a) {
	return {$: 'PidKi', a: a};
};
var $author$project$Controller$PIDeq$PidKp = function (a) {
	return {$: 'PidKp', a: a};
};
var $author$project$Controller$PIDeq$PidUeq = function (a) {
	return {$: 'PidUeq', a: a};
};
var $author$project$Controller$PIDeq$view = F2(
	function (model, msgToMainMsg) {
		var ueqStr = function ($) {
			return $.ueqStr;
		}(model);
		var kpStr = function ($) {
			return $.kpStr;
		}(model);
		var kiStr = function ($) {
			return $.kiStr;
		}(model);
		var kdStr = function ($) {
			return $.kdStr;
		}(model);
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$spacing(10),
					$mdgriffith$elm_ui$Element$padding(20),
					$mdgriffith$elm_ui$Element$centerX
				]),
			_List_fromArray(
				[
					$author$project$UI$heading('Controller'),
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$spacing(20)
						]),
					_List_fromArray(
						[
							A3(
							$author$project$UI$textField,
							kpStr,
							'Kp',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Controller$PIDeq$PidKp)),
							A3(
							$author$project$UI$textField,
							kiStr,
							'Ki',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Controller$PIDeq$PidKi)),
							A3(
							$author$project$UI$textField,
							kdStr,
							'Kd',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Controller$PIDeq$PidKd))
						])),
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[$mdgriffith$elm_ui$Element$centerX]),
					_List_fromArray(
						[
							A3(
							$author$project$UI$textField,
							ueqStr,
							'Ueq',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Controller$PIDeq$PidUeq))
						]))
				]));
	});
var $author$project$Controller$view = F2(
	function (model, msgToMainMsg) {
		if (model.$ === 'PidModel') {
			var pidModel = model.a;
			return A2(
				$author$project$Controller$PID$view,
				pidModel,
				A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Controller$PidMsg));
		} else {
			var pideqModel = model.a;
			return A2(
				$author$project$Controller$PIDeq$view,
				pideqModel,
				A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Controller$PideqMsg));
		}
	});
var $author$project$ModelSystem$Level2Msg = function (a) {
	return {$: 'Level2Msg', a: a};
};
var $author$project$ModelSystem$LevelMsg = function (a) {
	return {$: 'LevelMsg', a: a};
};
var $author$project$ModelSystem$Level$Ag = function (a) {
	return {$: 'Ag', a: a};
};
var $author$project$ModelSystem$Level$Ap = function (a) {
	return {$: 'Ap', a: a};
};
var $author$project$ModelSystem$Level$H0 = function (a) {
	return {$: 'H0', a: a};
};
var $author$project$ModelSystem$Level$view = F2(
	function (model, msgToMainMsg) {
		var h0Str = function ($) {
			return $.h0Str;
		}(model);
		var apStr = function ($) {
			return $.apStr;
		}(model);
		var agStr = function ($) {
			return $.agStr;
		}(model);
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$spacing(10),
					$mdgriffith$elm_ui$Element$alignTop
				]),
			_List_fromArray(
				[
					$author$project$UI$heading('Level'),
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$spacing(30)
						]),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Element$column,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$alignTop,
									$mdgriffith$elm_ui$Element$spacing(10)
								]),
							_List_fromArray(
								[
									A3(
									$author$project$UI$textField,
									h0Str,
									'Hini',
									A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$Level$H0))
								])),
							A2(
							$mdgriffith$elm_ui$Element$column,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$alignTop,
									$mdgriffith$elm_ui$Element$spacing(10)
								]),
							_List_fromArray(
								[
									A3(
									$author$project$UI$textField,
									agStr,
									'A ',
									A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$Level$Ag)),
									A3(
									$author$project$UI$textField,
									apStr,
									'a ',
									A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$Level$Ap))
								]))
						]))
				]));
	});
var $author$project$ModelSystem$Level2$Ag1 = function (a) {
	return {$: 'Ag1', a: a};
};
var $author$project$ModelSystem$Level2$Ag2 = function (a) {
	return {$: 'Ag2', a: a};
};
var $author$project$ModelSystem$Level2$Ap1 = function (a) {
	return {$: 'Ap1', a: a};
};
var $author$project$ModelSystem$Level2$Ap2 = function (a) {
	return {$: 'Ap2', a: a};
};
var $author$project$ModelSystem$Level2$H01 = function (a) {
	return {$: 'H01', a: a};
};
var $author$project$ModelSystem$Level2$H02 = function (a) {
	return {$: 'H02', a: a};
};
var $author$project$ModelSystem$Level2$view = F2(
	function (model, msgToMainMsg) {
		var h02Str = function ($) {
			return $.h02Str;
		}(model);
		var h01Str = function ($) {
			return $.h01Str;
		}(model);
		var ap2Str = function ($) {
			return $.ap2Str;
		}(model);
		var ap1Str = function ($) {
			return $.ap1Str;
		}(model);
		var ag2Str = function ($) {
			return $.ag2Str;
		}(model);
		var ag1Str = function ($) {
			return $.ag1Str;
		}(model);
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$spacing(10),
					$mdgriffith$elm_ui$Element$padding(20),
					$mdgriffith$elm_ui$Element$centerX
				]),
			_List_fromArray(
				[
					$author$project$UI$heading('Level 2'),
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$spacing(35)
						]),
					_List_fromArray(
						[
							A3(
							$author$project$UI$textField,
							h01Str,
							'H1ini',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$Level2$H01)),
							A3(
							$author$project$UI$textField,
							ag1Str,
							'A1 ',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$Level2$Ag1)),
							A3(
							$author$project$UI$textField,
							ap1Str,
							'a1 ',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$Level2$Ap1))
						])),
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$spacing(35)
						]),
					_List_fromArray(
						[
							A3(
							$author$project$UI$textField,
							h02Str,
							'H2ini',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$Level2$H02)),
							A3(
							$author$project$UI$textField,
							ag2Str,
							'A2 ',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$Level2$Ag2)),
							A3(
							$author$project$UI$textField,
							ap2Str,
							'a2 ',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$Level2$Ap2))
						]))
				]));
	});
var $author$project$ModelSystem$view = F2(
	function (model, msgToMainMsg) {
		if (model.$ === 'LevelModel') {
			var levelModel = model.a;
			return A2(
				$author$project$ModelSystem$Level$view,
				levelModel,
				A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$LevelMsg));
		} else {
			var level2Model = model.a;
			return A2(
				$author$project$ModelSystem$Level2$view,
				level2Model,
				A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$ModelSystem$Level2Msg));
		}
	});
var $author$project$Reference$Step1Msg = function (a) {
	return {$: 'Step1Msg', a: a};
};
var $author$project$Reference$Step1$Step1FVal = function (a) {
	return {$: 'Step1FVal', a: a};
};
var $author$project$Reference$Step1$Step1IVal = function (a) {
	return {$: 'Step1IVal', a: a};
};
var $author$project$Reference$Step1$Step1TStep = function (a) {
	return {$: 'Step1TStep', a: a};
};
var $author$project$Reference$Step1$view = F2(
	function (model, msgToMainMsg) {
		var tStepStr = function ($) {
			return $.tStepStr;
		}(model);
		var iValStr = function ($) {
			return $.iValStr;
		}(model);
		var fValStr = function ($) {
			return $.fValStr;
		}(model);
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$spacing(10),
					$mdgriffith$elm_ui$Element$padding(20),
					$mdgriffith$elm_ui$Element$centerX
				]),
			_List_fromArray(
				[
					$author$project$UI$heading('Step Reference'),
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$spacing(25)
						]),
					_List_fromArray(
						[
							A3(
							$author$project$UI$textField,
							iValStr,
							'iVal',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Reference$Step1$Step1IVal)),
							A3(
							$author$project$UI$textField,
							tStepStr,
							'Tstep',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Reference$Step1$Step1TStep)),
							A3(
							$author$project$UI$textField,
							fValStr,
							'fVal',
							A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Reference$Step1$Step1FVal))
						]))
				]));
	});
var $author$project$Reference$view = F2(
	function (model, msgToMainMsg) {
		var stepParam = model.a;
		return A2(
			$author$project$Reference$Step1$view,
			stepParam,
			A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$Reference$Step1Msg));
	});
var $mdgriffith$elm_ui$Internal$Model$FontFamily = F2(
	function (a, b) {
		return {$: 'FontFamily', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Flag$fontFamily = $mdgriffith$elm_ui$Internal$Flag$flag(5);
var $elm$core$String$words = _String_words;
var $mdgriffith$elm_ui$Internal$Model$renderFontClassName = F2(
	function (font, current) {
		return _Utils_ap(
			current,
			function () {
				switch (font.$) {
					case 'Serif':
						return 'serif';
					case 'SansSerif':
						return 'sans-serif';
					case 'Monospace':
						return 'monospace';
					case 'Typeface':
						var name = font.a;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					case 'ImportFont':
						var name = font.a;
						var url = font.b;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
					default:
						var name = font.a.name;
						return A2(
							$elm$core$String$join,
							'-',
							$elm$core$String$words(
								$elm$core$String$toLower(name)));
				}
			}());
	});
var $mdgriffith$elm_ui$Element$Font$family = function (families) {
	return A2(
		$mdgriffith$elm_ui$Internal$Model$StyleClass,
		$mdgriffith$elm_ui$Internal$Flag$fontFamily,
		A2(
			$mdgriffith$elm_ui$Internal$Model$FontFamily,
			A3($elm$core$List$foldl, $mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'ff-', families),
			families));
};
var $mdgriffith$elm_ui$Internal$Model$OnlyDynamic = F2(
	function (a, b) {
		return {$: 'OnlyDynamic', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic = F2(
	function (a, b) {
		return {$: 'StaticRootAndDynamic', a: a, b: b};
	});
var $mdgriffith$elm_ui$Internal$Model$AllowHover = {$: 'AllowHover'};
var $mdgriffith$elm_ui$Internal$Model$Layout = {$: 'Layout'};
var $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle = {
	backgroundColor: $elm$core$Maybe$Nothing,
	borderColor: $elm$core$Maybe$Nothing,
	shadow: $elm$core$Maybe$Just(
		{
			blur: 0,
			color: A4($mdgriffith$elm_ui$Internal$Model$Rgba, 155 / 255, 203 / 255, 1, 1),
			offset: _Utils_Tuple2(0, 0),
			size: 3
		})
};
var $mdgriffith$elm_ui$Internal$Model$optionsToRecord = function (options) {
	var combine = F2(
		function (opt, record) {
			switch (opt.$) {
				case 'HoverOption':
					var hoverable = opt.a;
					var _v4 = record.hover;
					if (_v4.$ === 'Nothing') {
						return _Utils_update(
							record,
							{
								hover: $elm$core$Maybe$Just(hoverable)
							});
					} else {
						return record;
					}
				case 'FocusStyleOption':
					var focusStyle = opt.a;
					var _v5 = record.focus;
					if (_v5.$ === 'Nothing') {
						return _Utils_update(
							record,
							{
								focus: $elm$core$Maybe$Just(focusStyle)
							});
					} else {
						return record;
					}
				default:
					var renderMode = opt.a;
					var _v6 = record.mode;
					if (_v6.$ === 'Nothing') {
						return _Utils_update(
							record,
							{
								mode: $elm$core$Maybe$Just(renderMode)
							});
					} else {
						return record;
					}
			}
		});
	var andFinally = function (record) {
		return {
			focus: function () {
				var _v0 = record.focus;
				if (_v0.$ === 'Nothing') {
					return $mdgriffith$elm_ui$Internal$Model$focusDefaultStyle;
				} else {
					var focusable = _v0.a;
					return focusable;
				}
			}(),
			hover: function () {
				var _v1 = record.hover;
				if (_v1.$ === 'Nothing') {
					return $mdgriffith$elm_ui$Internal$Model$AllowHover;
				} else {
					var hoverable = _v1.a;
					return hoverable;
				}
			}(),
			mode: function () {
				var _v2 = record.mode;
				if (_v2.$ === 'Nothing') {
					return $mdgriffith$elm_ui$Internal$Model$Layout;
				} else {
					var actualMode = _v2.a;
					return actualMode;
				}
			}()
		};
	};
	return andFinally(
		A3(
			$elm$core$List$foldr,
			combine,
			{focus: $elm$core$Maybe$Nothing, hover: $elm$core$Maybe$Nothing, mode: $elm$core$Maybe$Nothing},
			options));
};
var $mdgriffith$elm_ui$Internal$Model$toHtml = F2(
	function (mode, el) {
		switch (el.$) {
			case 'Unstyled':
				var html = el.a;
				return html($mdgriffith$elm_ui$Internal$Model$asEl);
			case 'Styled':
				var styles = el.a.styles;
				var html = el.a.html;
				return A2(
					html,
					mode(styles),
					$mdgriffith$elm_ui$Internal$Model$asEl);
			case 'Text':
				var text = el.a;
				return $mdgriffith$elm_ui$Internal$Model$textElement(text);
			default:
				return $mdgriffith$elm_ui$Internal$Model$textElement('');
		}
	});
var $mdgriffith$elm_ui$Internal$Model$renderRoot = F3(
	function (optionList, attributes, child) {
		var options = $mdgriffith$elm_ui$Internal$Model$optionsToRecord(optionList);
		var embedStyle = function () {
			var _v0 = options.mode;
			if (_v0.$ === 'NoStaticStyleSheet') {
				return $mdgriffith$elm_ui$Internal$Model$OnlyDynamic(options);
			} else {
				return $mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic(options);
			}
		}();
		return A2(
			$mdgriffith$elm_ui$Internal$Model$toHtml,
			embedStyle,
			A4(
				$mdgriffith$elm_ui$Internal$Model$element,
				$mdgriffith$elm_ui$Internal$Model$asEl,
				$mdgriffith$elm_ui$Internal$Model$div,
				attributes,
				$mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[child]))));
	});
var $mdgriffith$elm_ui$Internal$Model$SansSerif = {$: 'SansSerif'};
var $mdgriffith$elm_ui$Internal$Model$Typeface = function (a) {
	return {$: 'Typeface', a: a};
};
var $mdgriffith$elm_ui$Internal$Model$rootStyle = function () {
	var families = _List_fromArray(
		[
			$mdgriffith$elm_ui$Internal$Model$Typeface('Open Sans'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Helvetica'),
			$mdgriffith$elm_ui$Internal$Model$Typeface('Verdana'),
			$mdgriffith$elm_ui$Internal$Model$SansSerif
		]);
	return _List_fromArray(
		[
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$bgColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'bg-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0)),
				'background-color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontColor,
			A3(
				$mdgriffith$elm_ui$Internal$Model$Colored,
				'fc-' + $mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1)),
				'color',
				A4($mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1))),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontSize,
			$mdgriffith$elm_ui$Internal$Model$FontSize(20)),
			A2(
			$mdgriffith$elm_ui$Internal$Model$StyleClass,
			$mdgriffith$elm_ui$Internal$Flag$fontFamily,
			A2(
				$mdgriffith$elm_ui$Internal$Model$FontFamily,
				A3($elm$core$List$foldl, $mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'font-', families),
				families))
		]);
}();
var $mdgriffith$elm_ui$Element$layoutWith = F3(
	function (_v0, attrs, child) {
		var options = _v0.options;
		return A3(
			$mdgriffith$elm_ui$Internal$Model$renderRoot,
			options,
			A2(
				$elm$core$List$cons,
				$mdgriffith$elm_ui$Internal$Model$htmlClass(
					A2(
						$elm$core$String$join,
						' ',
						_List_fromArray(
							[$mdgriffith$elm_ui$Internal$Style$classes.root, $mdgriffith$elm_ui$Internal$Style$classes.any, $mdgriffith$elm_ui$Internal$Style$classes.single]))),
				_Utils_ap($mdgriffith$elm_ui$Internal$Model$rootStyle, attrs)),
			child);
	});
var $mdgriffith$elm_ui$Element$layout = $mdgriffith$elm_ui$Element$layoutWith(
	{options: _List_Nil});
var $mdgriffith$elm_ui$Element$Font$sansSerif = $mdgriffith$elm_ui$Internal$Model$SansSerif;
var $mdgriffith$elm_ui$Element$Font$typeface = $mdgriffith$elm_ui$Internal$Model$Typeface;
var $author$project$UI$view = function (children) {
	return A2(
		$mdgriffith$elm_ui$Element$layout,
		_List_fromArray(
			[
				$mdgriffith$elm_ui$Element$width($mdgriffith$elm_ui$Element$fill),
				$mdgriffith$elm_ui$Element$Font$family(
				_List_fromArray(
					[
						$mdgriffith$elm_ui$Element$Font$typeface('IBM Plex Sans'),
						$mdgriffith$elm_ui$Element$Font$sansSerif
					]))
			]),
		A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$width(
					A2($mdgriffith$elm_ui$Element$maximum, 1060, $mdgriffith$elm_ui$Element$fill)),
					$mdgriffith$elm_ui$Element$paddingEach(
					{bottom: 20, left: 50, right: 50, top: 30}),
					$mdgriffith$elm_ui$Element$centerX,
					$mdgriffith$elm_ui$Element$Font$size(16),
					$mdgriffith$elm_ui$Element$Font$color(
					A3($mdgriffith$elm_ui$Element$rgb255, 80, 80, 80)),
					$mdgriffith$elm_ui$Element$spacing(50)
				]),
			children));
};
var $author$project$EdoSolver$Tfim = function (a) {
	return {$: 'Tfim', a: a};
};
var $author$project$EdoSolver$Tini = function (a) {
	return {$: 'Tini', a: a};
};
var $author$project$EdoSolver$view2 = F2(
	function (edoIStates, msgToMainMsg) {
		var tiniStr = function ($) {
			return $.tiniStr;
		}(edoIStates);
		var tfimStr = function ($) {
			return $.tfimStr;
		}(edoIStates);
		return A2(
			$mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					$mdgriffith$elm_ui$Element$spacing(0),
					$mdgriffith$elm_ui$Element$alignTop,
					$mdgriffith$elm_ui$Element$moveUp(5)
				]),
			_List_fromArray(
				[
					A3(
					$author$project$UI$textField,
					tiniStr,
					'Tini ',
					A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$EdoSolver$Tini)),
					A3(
					$author$project$UI$textField,
					tfimStr,
					'Tfin',
					A2($elm$core$Basics$composeL, msgToMainMsg, $author$project$EdoSolver$Tfim))
				]));
	});
var $author$project$Main$view = function (bigModel) {
	var model = $author$project$Main$modelFromBigModel(bigModel);
	var modelSystemModel = function ($) {
		return $.modelSystemModel;
	}(model);
	var refModel = function ($) {
		return $.refModel;
	}(model);
	var refFunc = $author$project$Reference$refFunctionFromModel(refModel);
	var edoParam = function ($) {
		return $.edoParam;
	}(model);
	var edoIStates = function ($) {
		return $.edoIStates;
	}(model);
	var controlModel = function ($) {
		return $.controlModel;
	}(model);
	var controller = $author$project$Controller$controllerFromModel(controlModel);
	var chartsModel = function ($) {
		return $.chartsModel;
	}(model);
	var chartData = function ($) {
		return $.chartData;
	}(model);
	var chartsTuple = A3($author$project$MyCharts$chartsTuple, chartsModel, chartData, $author$project$Main$MyChartsMsg);
	var _v0 = function () {
		if (chartData.b) {
			var cd = chartData.a;
			var cdlist = chartData.b;
			return _Utils_Tuple3(
				$author$project$DataConvert$xsFromDatum(cd),
				$author$project$DataConvert$rsFromDatum(cd),
				$author$project$DataConvert$usFromDatum(cd));
		} else {
			if (bigModel.$ === 'SolvingEdo') {
				var xlist = $author$project$ModelSystem$xsFromModel(modelSystemModel);
				var _v3 = A5(
					$author$project$EdoSolver$getRsUs,
					xlist,
					edoParam,
					$author$project$ModelSystem$output(model.modelSystemType),
					refFunc,
					controller);
				var rlist = _v3.a;
				var ulist = _v3.b;
				return _Utils_Tuple3(xlist, rlist, ulist);
			} else {
				var animationEdoParam = bigModel.b;
				var xsAnimation = bigModel.c;
				var _v4 = A5(
					$author$project$EdoSolver$getRsUs,
					xsAnimation,
					animationEdoParam,
					$author$project$ModelSystem$output(model.modelSystemType),
					refFunc,
					controller);
				var rlist = _v4.a;
				var ulist = _v4.b;
				return _Utils_Tuple3(xsAnimation, rlist, ulist);
			}
		}
	}();
	var xs = _v0.a;
	var rs = _v0.b;
	var us = _v0.c;
	return $author$project$UI$view(
		_Utils_ap(
			_List_fromArray(
				[
					A2(
					$mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							$mdgriffith$elm_ui$Element$alignTop,
							$mdgriffith$elm_ui$Element$alignLeft,
							$mdgriffith$elm_ui$Element$width(
							A2(
								$mdgriffith$elm_ui$Element$maximum,
								1000,
								A2($mdgriffith$elm_ui$Element$minimum, 880, $mdgriffith$elm_ui$Element$fill)))
						]),
					_List_fromArray(
						[
							A2(
							$mdgriffith$elm_ui$Element$column,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$spacing(0),
									$mdgriffith$elm_ui$Element$alignTop,
									$mdgriffith$elm_ui$Element$padding(0),
									$mdgriffith$elm_ui$Element$alignLeft
								]),
							_List_fromArray(
								[
									A2($author$project$ModelSystem$view, modelSystemModel, $author$project$Main$ModelSystemMsg),
									A2($author$project$Controller$view, controlModel, $author$project$Main$ControlMsg),
									A2($author$project$Reference$view, refModel, $author$project$Main$RefMsg),
									A2(
									$mdgriffith$elm_ui$Element$row,
									_List_fromArray(
										[
											$mdgriffith$elm_ui$Element$centerX,
											$mdgriffith$elm_ui$Element$spacing(40),
											$mdgriffith$elm_ui$Element$moveDown(5)
										]),
									_List_fromArray(
										[
											A2($author$project$EdoSolver$view2, edoIStates, $author$project$Main$EdoMsg),
											A2(
											$author$project$UI$button,
											'RunODE',
											$elm$core$Maybe$Just($author$project$Main$RunEdo)),
											A2(
											$author$project$UI$button,
											'Animation',
											$elm$core$Maybe$Just($author$project$Main$RunAnimation))
										]))
								])),
							A2(
							$mdgriffith$elm_ui$Element$el,
							_List_fromArray(
								[
									$mdgriffith$elm_ui$Element$alignRight,
									$mdgriffith$elm_ui$Element$centerY,
									$mdgriffith$elm_ui$Element$padding(10),
									$mdgriffith$elm_ui$Element$width(
									A2($mdgriffith$elm_ui$Element$minimum, 400, $mdgriffith$elm_ui$Element$shrink)),
									$mdgriffith$elm_ui$Element$height(
									A2(
										$mdgriffith$elm_ui$Element$maximum,
										400,
										A2($mdgriffith$elm_ui$Element$minimum, 400, $mdgriffith$elm_ui$Element$fill)))
								]),
							$mdgriffith$elm_ui$Element$html(
								A4($author$project$ModelSystem$simulation, xs, rs, us, modelSystemModel)))
						]))
				]),
			A2(
				$elm$core$List$map,
				function (_v5) {
					var a = _v5.a;
					var b = _v5.b;
					return A2(
						$mdgriffith$elm_ui$Element$row,
						_List_fromArray(
							[
								$mdgriffith$elm_ui$Element$width(
								A2(
									$mdgriffith$elm_ui$Element$maximum,
									1000,
									A2($mdgriffith$elm_ui$Element$minimum, 880, $mdgriffith$elm_ui$Element$fill)))
							]),
						_List_fromArray(
							[
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[$mdgriffith$elm_ui$Element$alignLeft]),
								a),
								A2(
								$mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[$mdgriffith$elm_ui$Element$alignRight]),
								b)
							]));
				},
				chartsTuple)));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));