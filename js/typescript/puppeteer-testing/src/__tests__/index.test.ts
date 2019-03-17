import path from 'path';
import { Browser, Page } from 'puppeteer';

import { getValue } from '..';

const page = (global as any).page as Page;
const browser = (global as any).browser as Browser;

const TEST_ASSETS_DIR = path.resolve(`${__dirname}/../../test-assets`);

describe('description', () => {
  test('returns the expected value', () => {
    expect(getValue()).toEqual(42);
  });
});

describe('Website', () => {
  const logs: string[] = [];
  beforeAll(async () => {
    await page.goto('https://ignaciocarbajo.com');
    page.on('console', msg => logs.push(msg.text()));
  });

  it('should be titled ', async () => {
    await expect(page.title()).resolves.toMatch('Ignacio Carbajo');
    await page.screenshot({path: `${TEST_ASSETS_DIR}/example.png`, fullPage: true});
  });

  test('some browser methods', async () => {
    await expect(browser.pages()).resolves.toHaveLength(2);
    await expect(browser.userAgent()).resolves.toMatch(new RegExp('Mozilla/[0-9.]* \\(X11; Linux' +
      ' x86_64\\) AppleWebKit/[0-9.]* \\(KHTML, like Gecko\\) HeadlessChrome/[0-9.]* Safari/[0-9.]*'));

    expect(browser.wsEndpoint()).toMatch(/^ws:\/\/127.0.0.1:[0-9]*\/devtools\/browser\/[0-9-a-z]*$/);

    const target = browser.target();

    expect(target.type()).toEqual('browser');
  });

  test('some page methods', async () => {
    const randomNum = Math.random().toString();

    await expect(page.content()).resolves.toContain('<body>');
    await expect(page.evaluate('typeof ga')).resolves.toEqual('function');
    await expect(page.url()).toEqual('https://ignaciocarbajo.com/');

    await page.evaluate(`console.log("${randomNum}")`);

    expect(logs[logs.length - 1]).toEqual(randomNum);
  });
});
