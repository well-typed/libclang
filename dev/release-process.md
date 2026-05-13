# `libclang-bindings` Release Process

## Prerequisites

* [ ] Decide on new version number (`MAJOR.MINOR.PATCH`)
    * Tag name: `release-${VERSION}`

## Preparation

* [ ] Set the version in `libclang-bindings.cabal`

* [ ] Set the `source-repository this` tag in `libclang-bindings.cabal`

* [ ] Set the version in `configure.ac`

* [ ] Reconfigure

    ```
    $ path/to/autoconf-2.71/bin/autoreconf -i
    ```

* [ ] Update the `CHANGELOG`
    * [ ] Set the version number
    * [ ] Set the release date (UTC)

* [ ] Prepare release notes

## GitHub

* [ ] Push `main`

    ```
    $ git push origin main
    ```

* [ ] Tag the release

    ```
    $ git tag "${TAG}" -m "Release ${VERSION}"
    ```

* [ ] Push the tag

    ```
    $ git push origin "${TAG}"
    ```

* [ ] [Create a new release](https://github.com/well-typed/libclang/releases/new)
    * [ ] Select tag
    * [ ] Target: `main`
    * [ ] Release title: `Release ${VERSION}`
    * [ ] Release notes: (paste release notes, formatted for GitHub Markdown)
    * [ ] Publish release

## Hackage

TBD

## Preparation for next release

* [ ] Update the `CHANGELOG`
    *  [ ] Add new section at top

        ```markdown
        ## ?.?.? -- YYYY-mm-dd

        ### Breaking changes

        ### New features

        ### Minor changes

        ### Bug fixes
        ```
