export const _copyST = m => () => new Map(m)

export const empty = new Map()

export const run = f => f()

export const _fmapMap = (m0, f) => {
  const m = {}
  for (let [k, v] of m0)
    m.set(k, f(v))
  return m
}

export function _mapWithKey(m0, f) {
  const m = new Map()
  for (const [k, v] of m0) 
    m.set(k, f(k)(v))
  return m
}

export const _foldM = bind => f => mz => m => {
  let acc = mz
  const g = k => v => z => f(z)(k)(v)
  for (let [k, v] of m) {
    acc = bind(acc)(g(k)(v))
  }
  return acc
}

export const _foldSCMap = (m, z, f, fromMaybe) => {
  let acc = z
  for (let [k, v] of m) {
    const maybeR = f(acc)(k)(v)
    const r = fromMaybe(null)(maybeR)
    if (r === null)
      return acc
    else acc = r
  }
  return acc
}

export const _all = f => m => {
  for (var k in m) {
    if (!f(k)(m.get(k))) return false
  }
  return true
}

export const size = m => m.size

export const _lookup = (no, yes, k, m) => m.has(k) ? yes(m.get(k)) : no

export const _lookupST = _lookup

export const toArrayWithKey = f => m => {
  const r = []
  for (let [k, v] of m) {
    r.push(f(k)(v))
  }
  return r
}

export const keys = m => [...m.keys()]

const newImpl = () => new Map()

export { newImpl as new }

export const peekImpl = (m, k, just, nothing) => m.has(k) ? just(m.get(k)) : nothing

export const pokeImpl = (m, k, v) => m.set(k, v)

export const deleteImpl = (m, k) => m.delete(k)