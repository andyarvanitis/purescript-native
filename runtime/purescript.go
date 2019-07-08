package purescript

//go:generate purs --version

import "sync"

type Any interface{}

type Fn = func(Any) Any
type EffFn = func() Any

type Once = sync.Once

type IGNORE_UNUSED_RUNTIME = bool

const Undefined = "undefined"

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

func Get(dict Any, key string) Any {
	d, _ := dict.(map[string]Any)
	return d[key]
}

func SafeGet(dict map[string]Any, key string) Any {
	value, ok := dict[key]
	if !ok {
		panic("Foreign value '" + key + "' not defined")
	}
	return value
}

func At(arr Any, index int) Any {
	a, _ := arr.([]Any)
	return a[index]
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
