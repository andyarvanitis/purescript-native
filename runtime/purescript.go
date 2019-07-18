package purescript

//go:generate purs --version

import "sync"

type Any interface{}

type Fn = func(Any) Any
type EffFn = func() Any
type Dict = map[string]Any

type Once = sync.Once

type Fn1 = func(Any) Any
type Fn2 = func(Any, Any) Any
type Fn3 = func(Any, Any, Any) Any
type Fn4 = func(Any, Any, Any, Any) Any
type Fn5 = func(Any, Any, Any, Any, Any) Any
type Fn6 = func(Any, Any, Any, Any, Any, Any) Any
type Fn7 = func(Any, Any, Any, Any, Any, Any, Any) Any
type Fn8 = func(Any, Any, Any, Any, Any, Any, Any, Any) Any
type Fn9 = func(Any, Any, Any, Any, Any, Any, Any, Any, Any) Any
type Fn10 = func(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) Any

const Undefined = "undefined"

var foreign = make(map[string]Dict)

func Foreign(key string) Dict {
	value, found := foreign[key]
	if !found {
		value = make(Dict)
		foreign[key] = value
	}
	return value
}

func Apply(f Any, args ...Any) Any {
	result := f
	for _, arg := range args {
		fn, _ := result.(Fn)
		result = fn(arg)
	}
	return result
}

func Run(f Any, args ...Any) Any {
	fn, _ := f.(EffFn)
	return fn()
}

func Get(dict map[string]Any, key string) Any {
	value, ok := dict[key]
	if !ok {
		panic("Foreign value '" + key + "' not found")
	}
	return value
}

func Contains(dict Any, key string) bool {
	d, _ := dict.(map[string]Any)
	_, found := d[key]
	return found
}

func Length(arr Any) Any {
	a, _ := arr.([]Any)
	return len(a)
}
