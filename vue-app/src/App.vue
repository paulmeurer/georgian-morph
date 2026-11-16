<template>
  <div class="app">
    <header>
      <h1>Kartuli :: Noun Features</h1>
      <button class="theme-toggle" @click="toggleTheme" :title="'Switch to ' + (isDark ? 'light' : 'dark') + ' mode'">
        {{ isDark ? '☀' : '☾' }}
      </button>
    </header>
    <NounFeatures />
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import NounFeatures from './components/NounFeatures.vue'

const isDark = ref(false)

onMounted(() => {
  const saved = localStorage.getItem('theme')
  if (saved) {
    isDark.value = saved === 'dark'
  } else {
    isDark.value = window.matchMedia('(prefers-color-scheme: dark)').matches
  }
  applyTheme()
})

function toggleTheme() {
  isDark.value = !isDark.value
  localStorage.setItem('theme', isDark.value ? 'dark' : 'light')
  applyTheme()
}

function applyTheme() {
  document.documentElement.setAttribute('data-theme', isDark.value ? 'dark' : 'light')
}
</script>

<style>
:root {
  --color-bg: #ffffff;
  --color-text: #1a1a2e;
  --color-primary: #004499;
  --color-row-alt: #ebf3ff;
  --color-row-hover: #d4e5ff;
  --color-border: #c8d6e5;
  --color-header-bg: #f0f4f8;
  --color-edit-bg: #fff8e1;
  --color-btn: #004499;
  --color-btn-text: #ffffff;
  --color-btn-danger: #c0392b;
  --color-btn-success: #27ae60;
  --color-input-border: #aab7c4;
  --font-georgian: 'BPG Glaho Regular', 'Noto Sans Georgian', 'DejaVu Sans', sans-serif;
  --font-ui: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
}

@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    --color-bg: #1a1a2e;
    --color-text: #e0e0e0;
    --color-primary: #5b9bd5;
    --color-row-alt: #252547;
    --color-row-hover: #2d2d5e;
    --color-border: #3a3a5c;
    --color-header-bg: #22223b;
    --color-edit-bg: #3a3520;
    --color-btn: #5b9bd5;
    --color-btn-text: #1a1a2e;
    --color-input-border: #555580;
  }
}

:root[data-theme="dark"] {
  --color-bg: #1a1a2e;
  --color-text: #e0e0e0;
  --color-primary: #5b9bd5;
  --color-row-alt: #252547;
  --color-row-hover: #2d2d5e;
  --color-border: #3a3a5c;
  --color-header-bg: #22223b;
  --color-edit-bg: #3a3520;
  --color-btn: #5b9bd5;
  --color-btn-text: #1a1a2e;
  --color-input-border: #555580;
}

* {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

body {
  background: var(--color-bg);
  color: var(--color-text);
  font-family: var(--font-ui);
  font-size: 14px;
  line-height: 1.4;
}

.app {
  max-width: 1400px;
  margin: 0 auto;
  padding: 16px;
}

header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 16px;
}

header h1 {
  color: var(--color-primary);
  font-size: 20px;
  font-weight: 600;
}

.theme-toggle {
  background: none;
  border: 1px solid var(--color-border);
  border-radius: 6px;
  font-size: 20px;
  padding: 4px 10px;
  cursor: pointer;
  color: var(--color-text);
  transition: background 0.2s;
}

.theme-toggle:hover {
  background: var(--color-row-alt);
}
</style>
