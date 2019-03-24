import { Browser, Page } from 'puppeteer';

const page = (global as any).page as Page;

beforeAll(async () => {
  await page.addScriptTag({
    path: __dirname + '/../../build/bundle.js'
  })
})

describe('app', () => {
  beforeEach(async () => {
    await page.evaluate(() => {
      const el = document.createElement('app-root')
      document.body.appendChild(el);
    })
  });

  afterEach(async () => {
    const greetingText = await page.evaluate(() => {
      while (document.body.firstChild) {
        document.body.removeChild(document.body.firstChild);
      }
    })
  });

  it('works', async () => {
    const greetingText = await page.evaluate(() => {
      const appRootEl = document.querySelector('app-root');

      const greetingEl = appRootEl!.shadowRoot!.querySelector('simple-greeting');

      return greetingEl!.shadowRoot!.innerHTML;
    })

    expect(greetingText).toContain('Hello, Everyone!');
  })
});
