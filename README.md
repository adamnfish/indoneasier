# Indoneasier

> https://adamnfish.github.io/indoneasier/

An interactive player aid for the board game
[Indonesia](https://boardgamegeek.com/boardgame/19777/indonesia).

### Merger calculator

Indoneasier's merger calculator can take the place of a mergers cost
lookup table, printed or electronic. The goal is to handle the details
of merger calculations, leaving the players free to consider the strategy
of the merge.

### Player aids

Rules summaries are included to help while playing the game. These are
designed to serve as a refresher for someone returning the game, and to
help with the flow of a game that is in progress.

## Thanks

### Indonesia

[Indonesia](https://boardgamegeek.com/boardgame/19777/indonesia) is a
board game designed by Jeroen Doumen & Joris Wiersinga and published by
[Splotter Spellen](https://www.splottershop.com/).

This application is offered free for personal use for people playing an
official copy of the game. It is not affiliated with the board game.

### Icons

The company icons are modified versions of those found in a
fan-made alternative Indonesia map
[available for download on BGG](https://boardgamegeek.com/filepage/174618/indonesia-revised-vector-map-v22).
Thanks to all involved for the hard work creating vector versions of the
assets.

## Code overview

Indoneasier is a static webapp written in [Elm](https://elm-lang.org/).
The source code is in the [`src`](src) directory and public assets are in
the [`public`](public) directory.

The project uses [Parcel](https://parceljs.org/) as its build tool.

```bash
npm install        # install dependencies
npm start          # start the dev server
npm run build      # production build (output in dist/)
```

## End-to-end tests

Browser-based end-to-end tests live in the [`e2e`](e2e) directory, using
[Playwright](https://playwright.dev/). They run the full merger calculator
flow and capture screenshots at key steps, across a range of emulated
devices (phones, tablet, desktop).

To run the tests, first install the Playwright browser (one-time setup):

```bash
npx playwright install --with-deps chromium
```

Then build the app and run the tests:

```bash
npm run build          # tests run against the production build
npm run test:e2e       # run the tests
npm run test:e2e:report  # open the HTML report with screenshots
```

Screenshots are attached to each test in the HTML report. The CI workflow
uploads the report as a build artifact on every push.
