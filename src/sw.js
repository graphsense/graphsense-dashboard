import Level from 'level'
import Logger from './logger.js'

const logger = Logger.create('Service Worker')
const db = Level('cache')

const myFetch = (request) => {
  logger.debug('fetching', request)
  if (!db.isOpen()) {
    logger.warn('db not open')
    return db.on('open').then(myFetch(request))
  }
  return db.get(request.url)
    .then((resp) => {
      logger.debug(`${request.url} found in cache`)
      const headers = new Headers() // eslint-disable-line no-undef
      headers.append('Content-Type', 'application/json')
      return new Response(resp, { status: 200, statusText: 'OK', headers }) // eslint-disable-line no-undef
    }, (err) => { // eslint-disable-line 
      logger.debug(`${request.url} not found in cache, fetching it remotely`)
      return fetch(request) // eslint-disable-line no-undef
        .then(response => {
          // need to clone the response in order to use it twice in the following
          // see https://developer.mozilla.org/en-US/docs/Web/API/Response/clone
          const responseClone = response.clone()
          logger.debug('fetched', response)
          if (response.headers.get('Content-Type') === 'application/json') {
            response.text().then(text => {
              const prune = text.replace(/\s+/g, '')
              if (prune.startsWith('{"message"') ||
                  prune.startsWith('{"msg"') ||
                  prune.startsWith('{"Auth"') ||
                  prune.startsWith('{"status"')
              ) {
                logger.debug('starts with message, don\'t cache ' + text)
                return
              }
              db.put(request.url, text)
            })
          }
          return responseClone
        })
    })
}

self.addEventListener('fetch', (e) => { // eslint-disable-line no-undef
  e.respondWith(myFetch(e.request))
})
