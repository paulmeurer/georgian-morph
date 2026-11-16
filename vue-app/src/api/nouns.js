const API_BASE = '/kartuli/api'

export async function searchNouns({ stem, code, pos, lang, features, styleFeatures }) {
  const params = new URLSearchParams()
  if (stem) params.set('search-stem', stem)
  if (code) params.set('search-code', code)
  if (pos) params.set('search-pos', pos)
  if (lang) params.set('search-lang', lang)
  if (features) params.set('search-features', features)
  if (styleFeatures) params.set('search-style-features', styleFeatures)
  const res = await fetch(`${API_BASE}/noun-features?${params}`)
  if (!res.ok) throw new Error(`HTTP ${res.status}`)
  return res.json()
}

export async function getChoices(type) {
  const params = new URLSearchParams({ type })
  const res = await fetch(`${API_BASE}/noun-feature-choices?${params}`)
  if (!res.ok) throw new Error(`HTTP ${res.status}`)
  return res.json()
}

export async function updateNoun({ action, stemKey, stem, code, pos, features, styleFeatures, lang, template, comment }) {
  const params = new URLSearchParams({
    action,
    'stem-key': stemKey,
    stem: stem ?? '-',
    code: code ?? '-',
    pos: pos ?? '-',
    features: features ?? '-',
    'style-features': styleFeatures ?? '-',
    lang: lang ?? '*',
    template: template ?? '-',
    comment: comment ?? '-'
  })
  const res = await fetch(`${API_BASE}/noun-update?${params}`)
  if (!res.ok) throw new Error(`HTTP ${res.status}`)
  return res.json()
}
