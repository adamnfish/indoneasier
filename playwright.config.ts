import { defineConfig, devices } from '@playwright/test';

export default defineConfig({
  testDir: './e2e/tests',
  outputDir: './e2e/test-results',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 1 : 0,
  reporter: [
    ['html', { outputFolder: './e2e/playwright-report', open: 'never' }],
    ['list'],
  ],
  use: {
    baseURL: 'http://localhost:4000',
    screenshot: 'only-on-failure',
    trace: 'retain-on-failure',
    reducedMotion: 'reduce',
  },
  webServer: {
    command: 'npx serve ./dist -p 4000 -s',
    url: 'http://localhost:4000',
    reuseExistingServer: !process.env.CI,
    timeout: 30_000,
  },
  projects: [
    // iOS/iPadOS devices default to WebKit in Playwright's presets; override to Chromium
    // since we only install Chromium. Viewport, UA, and pixel ratio are preserved.
    { name: 'iPhone SE',         use: { ...devices['iPhone SE'],         browserName: 'chromium' } },
    { name: 'iPhone 15 Pro Max', use: { ...devices['iPhone 15 Pro Max'], browserName: 'chromium' } },
    { name: 'Pixel 5',           use: { ...devices['Pixel 5'] } },
    { name: 'Galaxy S8+',        use: { ...devices['Galaxy S8+'] } },
    { name: 'iPad Pro 11',       use: { ...devices['iPad Pro 11'],       browserName: 'chromium' } },
    { name: 'Desktop Chrome',    use: { viewport: { width: 1280, height: 800 } } },
  ],
});
