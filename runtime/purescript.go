package purescript

//go:generate purs --version

type Any = interface{}

type Fn = func(Any) Any
type EffFn = func() Any

func Apply(f Any, args ...Any) Any {
	result := f
	for _, arg := range args {
		fn, _ := result.(Fn)
		result = fn(arg)
	}
	return result
}

func Get(dict Any, key string) Any {
	d, _ := dict.(func(string) Any)
	return d(key)
}
