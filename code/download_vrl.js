#!/usr/bin/env node
// Downloads VRL JSON files using a real browser session (bypasses Cloudflare).
// Reads VRL_EMAIL and VRL_PASSWORD from environment variables.
// Usage: node code/download_vrl.js
// Called from R via: system2("node", "code/download_vrl.js")

const { chromium } = require('/Users/josephloffredo/.npm/_npx/9833c18b2d85bc59/node_modules/playwright');
const fs = require('fs');
const path = require('path');

const OUT_DIR = path.join(__dirname, '..', 'data', 'vrl_cache');
const BASE_URL = 'https://tracker.votingrightslab.org';
const YEARS = [2021, 2022, 2023, 2024, 2025, 2026];

async function run() {
  const email = process.env.VRL_EMAIL;
  const password = process.env.VRL_PASSWORD;

  if (!email || !password) {
    console.error('ERROR: VRL_EMAIL and VRL_PASSWORD must be set in environment');
    process.exit(1);
  }

  fs.mkdirSync(OUT_DIR, { recursive: true });

  const browser = await chromium.launch({
    headless: false,
    executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
  });
  const context = await browser.newContext({
    userAgent: 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36'
  });
  const page = await context.newPage();

  // Bypass webdriver detection
  await page.addInitScript(() => {
    Object.defineProperty(navigator, 'webdriver', { get: () => undefined });
  });

  // Log in
  console.log('Logging in...');
  await page.goto(`${BASE_URL}/login`, { waitUntil: 'load', timeout: 60000 });
  await page.waitForTimeout(5000);
  await page.locator('input[placeholder="Work Email"]').first().fill(email, { force: true });
  await page.locator('input[placeholder="Password"]').first().fill(password, { force: true });
  await page.locator('button:has-text("Log in")').first().click({ force: true });
  await page.waitForTimeout(5000);
  console.log('Logged in. Current URL:', page.url());

  // Download each year's bills file
  for (const year of YEARS) {
    const url = `${BASE_URL}/storage/bills-${year}.json`;
    console.log(`Fetching ${url}...`);
    const data = await page.evaluate(async (u) => {
      const r = await fetch(u, { credentials: 'include' });
      return r.ok ? r.text() : null;
    }, url);

    if (data) {
      fs.writeFileSync(path.join(OUT_DIR, `bills-${year}.json`), data);
      console.log(`  Saved bills-${year}.json (${data.length} chars)`);
    } else {
      console.error(`  FAILED to fetch bills-${year}.json`);
    }
  }

  // Download tags
  console.log('Fetching tags.json...');
  const tagsData = await page.evaluate(async (u) => {
    const r = await fetch(u, { credentials: 'include' });
    return r.ok ? r.text() : null;
  }, `${BASE_URL}/storage/tags.json`);

  if (tagsData) {
    fs.writeFileSync(path.join(OUT_DIR, 'tags.json'), tagsData);
    console.log(`  Saved tags.json (${tagsData.length} chars)`);
  } else {
    console.error('  FAILED to fetch tags.json');
  }

  await browser.close();
  console.log('Done.');
}

run().catch(err => {
  console.error('Fatal error:', err.message);
  process.exit(1);
});
