import { test, expect, Page, TestInfo } from '@playwright/test';

async function shot(page: Page, testInfo: TestInfo, step: string, fullPage = false) {
  const screenshot = await page.screenshot({ fullPage });
  await testInfo.attach(step, { body: screenshot, contentType: 'image/png' });
}

test.describe('Merger flow', () => {
  test('single owner: Rice, size 4', async ({ page }, testInfo) => {
    // Welcome
    await page.goto('/');
    await expect(page.locator('.merger--list')).toBeVisible();
    await shot(page, testInfo, '01-welcome', true);

    // Select Rice (exact match to avoid matching "Rice / spice")
    await page.locator('.merger-item--container').filter({
      has: page.locator('.merger-item--name', { hasText: /^Rice$/ }),
    }).click();
    await expect(page.locator('.size--list')).toBeVisible();
    await shot(page, testInfo, '02-company-size');

    // Select size 4 (exact match to avoid matching 14, 24, 34)
    await page.locator('.size-item--button').filter({
      has: page.locator('.size-item--number', { hasText: /^4$/ }),
    }).click();
    await expect(page.locator('.split--list')).toBeVisible();
    await shot(page, testInfo, '03-company-split');

    // Select single owner
    await page.locator('.split-item--button', { hasText: 'Single' }).click();
    await expect(page.locator('.bid--list')).toBeVisible();
    await shot(page, testInfo, '04-cost-table');

    // Select first (minimum) bid
    await page.locator('.bid-item--button').first().click();
    await expect(page.locator('.payment-rows')).toBeVisible();
    await shot(page, testInfo, '05-payments');

    // Close merger
    await page.locator('.finish-merger--button').click();
    await expect(page.locator('.merger--list')).toBeVisible();
    await shot(page, testInfo, '06-back-to-welcome', true);
  });

  test('split owner: Oil, size 6, 1/5 split', async ({ page }, testInfo) => {
    // Welcome
    await page.goto('/');
    await expect(page.locator('.merger--list')).toBeVisible();

    // Select Oil
    await page.locator('.merger-item--container').filter({
      has: page.locator('.merger-item--name', { hasText: /^Oil$/ }),
    }).click();
    await expect(page.locator('.size--list')).toBeVisible();

    // Select size 6
    await page.locator('.size-item--button').filter({
      has: page.locator('.size-item--number', { hasText: /^6$/ }),
    }).click();
    await expect(page.locator('.split--list')).toBeVisible();
    await shot(page, testInfo, '03-split-owner-split-select');

    // Select first ratio split (1/5) — index 1 skips "Single owner" at index 0
    await page.locator('.split-item--button').nth(1).click();
    await expect(page.locator('.bid--list')).toBeVisible();
    await shot(page, testInfo, '04-split-owner-cost-table');

    // Select first (minimum) bid
    await page.locator('.bid-item--button').first().click();
    await expect(page.locator('.payment-rows')).toBeVisible();
    // Verify two payment rows are shown
    await expect(page.locator('.payment-row')).toHaveCount(2);
    await shot(page, testInfo, '05-split-owner-payments');

    // Close merger
    await page.locator('.finish-merger--button').click();
    await expect(page.locator('.merger--list')).toBeVisible();
  });
});
