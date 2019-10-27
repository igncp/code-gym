# Tutorial notes

## Introduction

- Progressive framework
- Version is `2.x`
- Has libraries for more features: Core, Vuex, Vue-Router
- Creator: Evan You
- Has directives: `v-for`, `v-if`, `v-model`, `v-bind`, `v-on`
- Has lifecycle: `created`
- Has computed data: `computed`
- Has a Chrome extension
- Has a CLI: `@vue/cli`
- `.vue` files
- Vue components are different than Web Spec Custom Elements
- `vm` stands for `viewmodel`
- Directives have modifiers (after a `.`)
- Computed properties can have setter or getter

## Links

- Awesome Vue: https://github.com/vuejs/awesome-vue
- Repo: https://github.com/vuejs/vue
- Roadmap: https://github.com/vuejs/vue/projects/6
- Advanced Vue: https://github.com/filrak/vuejs-advanced-learning
- Patterns: https://learn-vuejs.github.io/vue-patterns/patterns
- Components Communication: https://alligator.io/vuejs/component-communication/
- CLI: https://cli.vuejs.org/guide/
- API: https://vuejs.org/v2/api/
- Style Guide: https://vuejs.org/v2/style-guide/
- Forum: https://forum.vuejs.org/
- Discord: https://discordapp.com/invite/HBherRA
- Chrome Extension: https://chrome.google.com/webstore/detail/vuejs-devtools/nhdogjmejiglipccpnnnanhbledajbpd
- Test Utils: https://vue-test-utils.vuejs.org
- 'vue-property-decorator' TS library: https://github.com/kaorun343/vue-property-decorator
- Enterprise Boilerplate: https://github.com/chrisvfritz/vue-enterprise-boilerplate
- Nuxt Framework: https://nuxtjs.org/
- Template Wemake: https://github.com/wemake-services/wemake-vue-template
- Vue Hackernews: https://github.com/vuejs/vue-hackernews
- Plugins: https://vuejs.org/v2/guide/plugins.html

## Links - Articles

All the articles listed here have been read:

### Performance

- 5 recommendations: https://dev.to/veebuv/5-extremely-easy-ways-to-drastically-improve-your-vuejs-app-s-speed-5k0
- Measure performance: https://vuedose.tips/tips/measure-runtime-performance-in-vue-js-apps/

### Misc

- Dependency Injection: https://markus.oberlehner.net/blog/dependency-injection-in-vue-applications/
- Redux vs Vuex:
    - https://medium.com/@Musclenun/redux-vs-vuex-9b682529c36#targetText=VUEX%3A,rendered%20when%20the%20state%20changes.
    - https://www.codementor.io/petarvukasinovic/redux-vs-vuex-for-state-management-in-vue-js-n10yd7g2f

## Notes

- To use TS, add `lang="ts"` inside the `<script>` tag
- Definite assignment assertion (example 5): https://devblogs.microsoft.com/typescript/announcing-typescript-2-7/
- When using `.vue` files, the templates strings are not supported (at least in tests).
    - Can use render functions or different file, or use a build with the compiler

## Strategy

- Follow a TDD approach for learning: Vuejs, Vuex, Nuxt, Vuejs routing and Vuejs core
- Start with premade template setups and progressively add customizations
- Add some Storybook stories
