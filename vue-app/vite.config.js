import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

export default defineConfig({
  plugins: [vue()],
  base: '/kartuli/app/',
    server: {
        proxy: {
            '/kartuli/api': {
                target: 'http://127.0.0.1:8004',
                changeOrigin: true
            }
        },
        allowedHosts: ['gekko.dyndns.org']
    }
})
