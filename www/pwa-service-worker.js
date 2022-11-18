// Establish Cache For Offline Use

const staticCacheName = "site_static";
const dynamicCache = "site_dynamic_cache_V1"
const assets = [
  "/",
  "jquery-3.6.0/jquery.min.js",
  "shiny-sass-1.7.3/shiny-sass.css"
  "shiny-javascript-1.7.3/shiny.min.js",
  "bootstrap-5.1.3/bootstrap.bundle.min.js",
  "bootstrap-5.1.3/bootstrap.min.css",
  "Lato-0.4.2/font.css",
  "Oswald-0.4.2/font.css",
  "bs3compat-0.4.1/transition.js",
  "bs3compat-0.4.1/tabs.js",
  "bs3compat-0.4.1/bs3compat.js",
  "selectize-0.12.4/selectize.css",
  "selectize-0.12.4/selectize.min.js",
  "selectize-0.12.4/selectize-plugin-a11y.min.js",
  "ionrangeslider-javascript-2.3.1/js/ion.rangeSlider.min.js",
  "strftime-0.9.2/strftime-min.js",
  "ionRangeSlider-2.3.1/ionRangeSlider.css",
  "font-awesome-6.2.0/css/all.min.css",
  "font-awesome-6.2.0/css/v4-shims.min.css",
  "shiny-javascript-1.7.3/shiny.min.js"
];



self.addEventListener("install", evt => {
  evt.waitUntil(
    caches.open(staticCacheName).then(cache => {
      console.log('caching essential assets');
      cache.addAll(assets);
     })
  );
});


self.addEventListener("activate", evt => {
  console.log('Something is Happening!');
});


self.addEventListener('fetch', function (event) {
  event.respondWith(
    fetch(event.request).catch(function () {
      return caches.match(event.request);
    }),
  );
});
        
        
 //       .then(fetchRes => {
 //         return caches.open(dynamicCache).then(cache => {
 //         cache.put(evt.request.url, fetchRes.clone());
 //         return fetchRes;
 //       })
 //   });
 // })
 // );
//});