<template>
  <div class="noun-features">
    <div class="search-bar">
      <label for="search-stem">Stem:</label>
      <input
        id="search-stem"
        ref="searchInput"
        v-model="searchStem"
        type="text"
        spellcheck="false"
        autocomplete="off"
        placeholder="use * as wildcard"
        @input="onSearchInput"
        @keydown="onSearchKeydown"
      />
      <label for="search-code">Code:</label>
      <select id="search-code" v-model="searchCode" @change="onSearchInput">
        <option value="">All</option>
        <option v-for="c in codeChoices" :key="c.value" :value="c.value">{{ c.value }}</option>
      </select>
      <label for="search-pos">POS:</label>
      <select id="search-pos" v-model="searchPos" @change="onSearchInput">
        <option value="">All</option>
        <option v-for="p in posChoices" :key="p.value" :value="p.value">{{ p.value }}</option>
      </select>
      <label for="search-lang">Lang:</label>
      <select id="search-lang" v-model="searchLang" @change="onSearchInput">
        <option value="">All</option>
        <option v-for="l in langChoices" :key="l.value" :value="l.value">{{ l.value }}</option>
      </select>
      <label for="search-features">Features:</label>
      <select id="search-features" v-model="searchFeatures" @change="onSearchInput">
        <option value="">All</option>
        <option v-for="f in featureAtoms" :key="f" :value="f">{{ f }}</option>
      </select>
      <label for="search-style-features">Style:</label>
      <select id="search-style-features" v-model="searchStyleFeatures" @change="onSearchInput">
        <option value="">All</option>
        <option v-for="sf in styleFeatureAtoms" :key="sf" :value="sf">{{ sf }}</option>
      </select>
      <span v-if="resultCount !== null" class="result-count">
        {{ resultCount }} {{ resultCount === 1 ? 'entry' : 'entries' }} found
        <span v-if="resultCount >= 1000">(max. 1000)</span>
      </span>
    </div>

    <div v-if="loading" class="loading">Loading...</div>

    <div v-if="nouns.length > 0" class="table-wrap">
      <table>
        <thead>
          <tr>
            <th>Stem</th>
            <th>Code</th>
            <th>POS</th>
            <th>SubId</th>
            <th>Features</th>
            <th>StyleFeatures</th>
            <th>Lang</th>
            <th>Comment</th>
            <th>Edit</th>
            <th>Date</th>
            <th>Translation</th>
            <th>Author</th>
          </tr>
        </thead>
        <tbody>
          <tr
            v-for="(noun, rowIdx) in nouns"
            :key="noun.stemKey"
            :class="{
              'row-alt': rowIdx % 2 === 0,
              'row-editing': editingRow === rowIdx
            }"
          >
            <!-- Stem -->
            <td
              :class="['cell-georgian', { 'cell-active': isActive(rowIdx, 0) }]"
              @click="activateCell(rowIdx, 0)"
            >
              <input
                v-if="isActive(rowIdx, 0)"
                :ref="el => setCellRef(rowIdx, 0, el)"
                v-model="editValues.stem"
                type="text"
                spellcheck="false"
                class="edit-input edit-input-georgian"
                @keydown="onCellKeydown($event, rowIdx, 0)"
              />
              <span v-else :class="{ 'cell-edited': editingRow === rowIdx && editValues.stem !== noun.stem }">{{ editingRow === rowIdx ? editValues.stem : noun.stem }}</span>
            </td>

            <!-- Code -->
            <td
              :class="{ 'cell-active': isActive(rowIdx, 1) }"
              @click="activateCell(rowIdx, 1)"
            >
              <select
                v-if="isActive(rowIdx, 1)"
                :ref="el => setCellRef(rowIdx, 1, el)"
                v-model="editValues.code"
                class="edit-select"
                @keydown="onCellKeydown($event, rowIdx, 1)"
              >
                <option v-for="c in codeChoices" :key="c.value" :value="c.value">
                  {{ c.value }}{{ c.example ? ' - ' + c.example : '' }}
                </option>
              </select>
              <span v-else :class="{ 'cell-edited': editingRow === rowIdx && editValues.code !== noun.code }">{{ editingRow === rowIdx ? editValues.code : noun.code }}</span>
            </td>

            <!-- POS -->
            <td
              :class="{ 'cell-active': isActive(rowIdx, 2) }"
              @click="activateCell(rowIdx, 2)"
            >
              <select
                v-if="isActive(rowIdx, 2)"
                :ref="el => setCellRef(rowIdx, 2, el)"
                v-model="editValues.pos"
                class="edit-select"
                @keydown="onCellKeydown($event, rowIdx, 2)"
              >
                <option v-for="p in posChoices" :key="p.value" :value="p.value">{{ p.value }}</option>
              </select>
              <span v-else :class="{ 'cell-edited': editingRow === rowIdx && editValues.pos !== noun.pos }">{{ editingRow === rowIdx ? editValues.pos : noun.pos }}</span>
            </td>

            <!-- SubId (read-only) -->
            <td>{{ noun.subId }}</td>

            <!-- Features -->
            <td
              :class="{ 'cell-active': isActive(rowIdx, 3) }"
              @click="activateCell(rowIdx, 3)"
            >
              <div
                v-if="isActive(rowIdx, 3)"
                :ref="el => setCellRef(rowIdx, 3, el)"
                class="sf-popup"
                tabindex="0"
                @keydown="onCellKeydown($event, rowIdx, 3)"
              >
                <div class="sf-checkboxes">
                  <label v-for="atom in featureAtoms" :key="atom" class="sf-label">
                    <input
                      type="checkbox"
                      :checked="editValues.features.includes('+' + atom)"
                      @change="toggleFeature('features', atom)"
                    />
                    {{ atom }}
                  </label>
                </div>
              </div>
              <span v-else :class="{ 'cell-edited': editingRow === rowIdx && editValues.features !== noun.features }">{{ editingRow === rowIdx ? editValues.features : noun.features }}</span>
            </td>

            <!-- StyleFeatures -->
            <td
              :class="{ 'cell-active': isActive(rowIdx, 4) }"
              @click="activateCell(rowIdx, 4)"
            >
              <div
                v-if="isActive(rowIdx, 4)"
                :ref="el => setCellRef(rowIdx, 4, el)"
                class="sf-popup"
                tabindex="0"
                @keydown="onCellKeydown($event, rowIdx, 4)"
              >
                <div class="sf-checkboxes">
                  <label v-for="atom in styleFeatureAtoms" :key="atom" class="sf-label">
                    <input
                      type="checkbox"
                      :checked="editValues.styleFeatures.includes('+' + atom)"
                      @change="toggleFeature('styleFeatures', atom)"
                    />
                    {{ atom }}
                  </label>
                </div>
              </div>
              <span v-else :class="{ 'cell-edited': editingRow === rowIdx && editValues.styleFeatures !== noun.styleFeatures }">{{ editingRow === rowIdx ? editValues.styleFeatures : noun.styleFeatures }}</span>
            </td>

            <!-- Lang -->
            <td
              :class="{ 'cell-active': isActive(rowIdx, 5) }"
              @click="activateCell(rowIdx, 5)"
            >
              <select
                v-if="isActive(rowIdx, 5)"
                :ref="el => setCellRef(rowIdx, 5, el)"
                v-model="editValues.lang"
                class="edit-select"
                @keydown="onCellKeydown($event, rowIdx, 5)"
              >
                <option v-for="l in langChoices" :key="l.value" :value="l.value">{{ l.value }}</option>
              </select>
              <span v-else :class="{ 'cell-edited': editingRow === rowIdx && editValues.lang !== noun.lang }">{{ editingRow === rowIdx ? editValues.lang : noun.lang }}</span>
            </td>

            <!-- Comment -->
            <td
              :class="{ 'cell-active': isActive(rowIdx, 6) }"
              @click="activateCell(rowIdx, 6)"
            >
              <textarea
                v-if="isActive(rowIdx, 6)"
                :ref="el => setCellRef(rowIdx, 6, el)"
                v-model="editValues.comment"
                rows="3"
                class="edit-textarea"
                @keydown="onCellKeydown($event, rowIdx, 6)"
              ></textarea>
              <span v-else :class="{ 'cell-edited': editingRow === rowIdx && editValues.comment !== (noun.comment || '') }">{{ editingRow === rowIdx ? editValues.comment : noun.comment }}</span>
            </td>

            <!-- Edit buttons -->
            <td class="edit-cell">
              <div v-if="editingRow === rowIdx" class="edit-buttons">
                <template v-if="confirmingDelete === rowIdx">
                  <span class="delete-confirm-text">Delete?</span>
                  <button class="btn btn-delete" @click="doAction('delete', noun)">Yes</button>
                  <button class="btn btn-cancel" @click="confirmingDelete = null">No</button>
                </template>
                <template v-else>
                  <button class="btn btn-add" @click="doAction('add', noun)" title="Add as new entry">Add</button>
                  <button class="btn btn-change" @click="doAction('change', noun)" title="Save changes (Ctrl+S)">Save</button>
                  <button class="btn btn-delete" @click="confirmingDelete = rowIdx" title="Delete entry">Delete</button>
                  <button class="btn btn-cancel" @click="cancelEdit" title="Cancel">Cancel</button>
                </template>
              </div>
            </td>

            <!-- Date (read-only) -->
            <td class="cell-small">{{ formatDate(noun.date) }}</td>

            <!-- Translation (read-only) -->
            <td>{{ noun.translation }}</td>

            <!-- Author (read-only) -->
            <td class="cell-small">{{ noun.author }}</td>
          </tr>
        </tbody>
      </table>
    </div>

    <div v-if="editingRow !== null" class="nav-hint">
      Ctrl+Arrows to navigate cells &middot; Enter or Ctrl+S to save &middot; Ctrl+A to add &middot; Escape to cancel
    </div>
  </div>
</template>

<script setup>
import { ref, reactive, onMounted, nextTick } from 'vue'
import { searchNouns, getChoices, updateNoun } from '../api/nouns.js'

const EDITABLE_COLS = ['stem', 'code', 'pos', 'features', 'styleFeatures', 'lang', 'comment']

const searchStem = ref('')
const searchCode = ref('')
const searchPos = ref('')
const searchLang = ref('')
const searchFeatures = ref('')
const searchStyleFeatures = ref('')
const nouns = ref([])
const resultCount = ref(null)
const loading = ref(false)
const searchInput = ref(null)

const editingRow = ref(null)
const activeCol = ref(null)
const confirmingDelete = ref(null)
const editValues = reactive({
  stem: '',
  code: '',
  pos: '',
  features: '',
  styleFeatures: '',
  lang: '',
  template: '',
  comment: ''
})

const cellRefs = {}

function setCellRef(row, col, el) {
  const key = `${row}-${col}`
  if (el) {
    cellRefs[key] = el
  } else {
    delete cellRefs[key]
  }
}

const codeChoices = ref([])
const posChoices = ref([])
const featureChoices = ref([])
const styleFeatureChoices = ref([])
const featureAtoms = ref([])
const styleFeatureAtoms = ref([])
const langChoices = ref([])
const templateChoices = ref([])

let searchTimeout = null

onMounted(async () => {
  searchInput.value?.focus()
  const [codes, pos, features, styleFeatures, langs, templates] = await Promise.all([
    getChoices('code'),
    getChoices('pos'),
    getChoices('features'),
    getChoices('styleFeatures'),
    getChoices('lang'),
    getChoices('template')
  ])
  codeChoices.value = codes.values
  posChoices.value = pos.values
  featureChoices.value = features.values
  const fAtoms = new Set()
  for (const v of features.values) {
    if (v.value && v.value !== '*') {
      for (const part of v.value.split('+')) {
        if (part) fAtoms.add(part)
      }
    }
  }
  featureAtoms.value = [...fAtoms].sort()
  styleFeatureChoices.value = styleFeatures.values
  const atoms = new Set()
  for (const v of styleFeatures.values) {
    if (v.value) {
      for (const part of v.value.split('+')) {
        if (part) atoms.add(part)
      }
    }
  }
  styleFeatureAtoms.value = [...atoms].sort()
  langChoices.value = langs.values
  templateChoices.value = templates.values
})

function onSearchInput() {
  clearTimeout(searchTimeout)
  searchTimeout = setTimeout(doSearch, 200)
}

function onSearchKeydown(e) {
  if (e.ctrlKey && e.key === 'ArrowDown' && nouns.value.length > 0) {
    e.preventDefault()
    activateCell(0, 0)
  }
}

function formatDate(d) {
  if (!d) return ''
  return d.split(' ')[0]
}

function toggleFeature(field, atom) {
  const current = editValues[field]
  const parts = current ? current.split('+').filter(p => p) : []
  const idx = parts.indexOf(atom)
  if (idx >= 0) {
    parts.splice(idx, 1)
  } else {
    parts.push(atom)
    parts.sort()
  }
  editValues[field] = parts.length ? '+' + parts.join('+') : ''
}

async function doSearch() {
  if (!searchStem.value && !searchCode.value && !searchPos.value && !searchLang.value && !searchFeatures.value && !searchStyleFeatures.value) {
    nouns.value = []
    resultCount.value = null
    return
  }
  loading.value = true
  try {
    const data = await searchNouns({
      stem: searchStem.value,
      code: searchCode.value,
      pos: searchPos.value,
      lang: searchLang.value,
      features: searchFeatures.value,
      styleFeatures: searchStyleFeatures.value
    })
    nouns.value = data.nouns
    resultCount.value = data.count
    editingRow.value = null
    activeCol.value = null
  } finally {
    loading.value = false
  }
}

function loadEditValues(rowIdx) {
  const noun = nouns.value[rowIdx]
  editValues.stem = noun.stem || ''
  editValues.code = noun.code || ''
  editValues.pos = noun.pos || ''
  editValues.features = noun.features || ''
  editValues.styleFeatures = noun.styleFeatures || ''
  editValues.lang = noun.lang || ''
  editValues.template = noun.template || ''
  editValues.comment = noun.comment || ''
}

function activateCell(rowIdx, colIdx) {
  if (editingRow.value !== rowIdx) {
    editingRow.value = rowIdx
    loadEditValues(rowIdx)
  }
  activeCol.value = colIdx
  nextTick(() => focusActiveCell(rowIdx, colIdx))
}

function focusActiveCell(row, col) {
  const el = cellRefs[`${row}-${col}`]
  if (el) el.focus()
}

function isActive(rowIdx, colIdx) {
  return editingRow.value === rowIdx && activeCol.value === colIdx
}

function cancelEdit() {
  editingRow.value = null
  activeCol.value = null
  confirmingDelete.value = null
  nextTick(() => searchInput.value?.focus())
}

function onCellKeydown(e, row, col) {
  if (e.key === 'Escape') {
    cancelEdit()
    e.preventDefault()
    return
  }
  if (e.key === 'Enter' && !e.ctrlKey) {
    doAction('change', nouns.value[row])
    e.preventDefault()
    return
  }

  if (e.ctrlKey) {
    if (e.key === 's' || e.key === 'S') {
      doAction('change', nouns.value[row])
      e.preventDefault()
      return
    }
    if (e.key === 'a' || e.key === 'A') {
      doAction('add', nouns.value[row])
      e.preventDefault()
      return
    }
  }

  if (!e.ctrlKey) return

  const arrows = ['ArrowRight', 'ArrowLeft', 'ArrowDown', 'ArrowUp']
  if (!arrows.includes(e.key)) return

  e.preventDefault()
  e.stopPropagation()

  let newRow = row
  let newCol = col

  switch (e.key) {
    case 'ArrowRight':
      newCol = Math.min(col + 1, EDITABLE_COLS.length - 1)
      break
    case 'ArrowLeft':
      newCol = Math.max(col - 1, 0)
      break
    case 'ArrowDown':
      newRow = Math.min(row + 1, nouns.value.length - 1)
      break
    case 'ArrowUp':
      if (row === 0) {
        cancelEdit()
        e.preventDefault()
        return
      }
      newRow = row - 1
      break
  }

  if (newRow !== row) {
    editingRow.value = newRow
    loadEditValues(newRow)
  }
  activeCol.value = newCol
  nextTick(() => focusActiveCell(newRow, newCol))
}

async function doAction(action, noun) {
  confirmingDelete.value = null
  try {
    const savedRow = editingRow.value
    await updateNoun({
      action,
      stemKey: noun.stemKey,
      stem: editValues.stem,
      code: editValues.code,
      pos: editValues.pos,
      features: editValues.features,
      styleFeatures: editValues.styleFeatures,
      lang: editValues.lang,
      template: editValues.template,
      comment: editValues.comment
    })
    editingRow.value = null
    activeCol.value = null
    await doSearch()
    if (action !== 'delete' && savedRow !== null && savedRow < nouns.value.length) {
      activateCell(savedRow, 0)
    }
  } catch (e) {
    alert('Error: ' + e.message)
  }
}
</script>

<style scoped>
.search-bar {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 12px;
  flex-wrap: wrap;
}

.search-bar label {
  font-weight: 600;
  color: var(--color-primary);
}

.search-bar input {
  font-size: 14px;
  padding: 6px 10px;
  border: 1px solid var(--color-input-border);
  border-radius: 4px;
  background: var(--color-bg);
  color: var(--color-text);
  width: 250px;
  font-family: var(--font-georgian);
}

.search-bar select {
  font-size: 14px;
  padding: 5px 8px;
  border: 1px solid var(--color-input-border);
  border-radius: 4px;
  background: var(--color-bg);
  color: var(--color-text);
}

.result-count {
  font-size: 13px;
  color: var(--color-primary);
}

.loading {
  color: var(--color-primary);
  padding: 20px 0;
}

.table-wrap {
  overflow-x: auto;
}

table {
  width: 100%;
  border-collapse: collapse;
  font-size: 13px;
}

thead th {
  position: sticky;
  top: 0;
  background: var(--color-header-bg);
  color: var(--color-primary);
  font-weight: 600;
  font-style: italic;
  padding: 6px 8px;
  text-align: left;
  white-space: nowrap;
  border-bottom: 2px solid var(--color-border);
  z-index: 1;
}

tbody td {
  padding: 4px 8px;
  border-bottom: 1px solid var(--color-border);
  vertical-align: top;
  cursor: default;
}

tbody tr:hover {
  background: var(--color-row-hover) !important;
}

.row-alt {
  background: var(--color-row-alt);
}

.row-editing {
  background: var(--color-edit-bg) !important;
}

.cell-active {
  position: relative;
}

.cell-georgian {
  font-family: var(--font-georgian);
  font-size: 14px;
}

.cell-small {
  font-size: 11px;
  color: #888;
  white-space: nowrap;
}

.cell-edited {
  color: var(--color-btn);
  font-weight: 600;
}

.sf-popup {
  position: absolute;
  top: 0;
  left: 0;
  z-index: 3;
  background: var(--color-bg);
  border: 1px solid var(--color-input-border);
  border-radius: 4px;
  box-shadow: 2px 2px 8px rgba(0,0,0,0.2);
  padding: 6px 8px;
  max-height: 300px;
  overflow-y: auto;
  outline: none;
}

.sf-checkboxes {
  display: flex;
  flex-wrap: wrap;
  gap: 2px 10px;
  min-width: 200px;
  max-width: 400px;
}

.sf-label {
  display: flex;
  align-items: center;
  gap: 3px;
  font-size: 12px;
  cursor: pointer;
  white-space: nowrap;
}

.sf-label input {
  margin: 0;
}

.edit-input {
  position: absolute;
  top: 2px;
  left: 2px;
  padding: 2px 4px;
  border: 1px solid var(--color-input-border);
  border-radius: 3px;
  font-size: 13px;
  background: var(--color-bg);
  color: var(--color-text);
  z-index: 2;
  box-shadow: 2px 2px 6px rgba(0,0,0,0.15);
}

.edit-textarea {
  position: absolute;
  top: 2px;
  left: 2px;
  min-width: 200px;
  padding: 2px 4px;
  border: 1px solid var(--color-input-border);
  border-radius: 3px;
  font-size: 13px;
  font-family: var(--font-ui);
  background: var(--color-bg);
  color: var(--color-text);
  z-index: 2;
  box-shadow: 2px 2px 6px rgba(0,0,0,0.15);
  resize: both;
}

.edit-input-georgian {
  font-family: var(--font-georgian);
  font-size: 14px;
}

.edit-select {
  position: absolute;
  top: 2px;
  left: 2px;
  width: auto;
  padding: 2px;
  border: 1px solid var(--color-input-border);
  border-radius: 3px;
  font-size: 13px;
  background: var(--color-bg);
  color: var(--color-text);
  z-index: 2;
  box-shadow: 2px 2px 6px rgba(0,0,0,0.15);
}

.edit-cell {
  min-width: 200px;
}

.delete-confirm-text {
  font-size: 12px;
  font-weight: 600;
  color: var(--color-btn-danger);
}

.edit-buttons {
  display: flex;
  gap: 4px;
  flex-wrap: nowrap;
  white-space: nowrap;
}

.btn {
  padding: 3px 8px;
  border: none;
  border-radius: 3px;
  font-size: 12px;
  cursor: pointer;
  font-weight: 500;
}

.btn-add {
  background: var(--color-btn-success);
  color: white;
}

.btn-change {
  background: var(--color-btn);
  color: var(--color-btn-text);
}

.btn-delete {
  background: var(--color-btn-danger);
  color: white;
}

.btn-cancel {
  background: #888;
  color: white;
}

.btn:hover {
  opacity: 0.85;
}

.nav-hint {
  margin-top: 8px;
  font-size: 12px;
  color: #888;
}
</style>
