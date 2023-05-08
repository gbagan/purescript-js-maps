const newImpl = () => new Map()

export { newImpl as new }

export const sizeImpl = m => m.size

export const peekImpl = (m, k, just, nothing) => m.has(k) ? just(m.get(k)) : nothing

export const pokeImpl = (m, k, v) => {m.set(k, v); return m}

export const deleteImpl = k => m => () => {m.delete(k); return m}