





<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
  <link rel="dns-prefetch" href="https://assets-cdn.github.com">
  <link rel="dns-prefetch" href="https://avatars0.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars1.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars2.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars3.githubusercontent.com">
  <link rel="dns-prefetch" href="https://github-cloud.s3.amazonaws.com">
  <link rel="dns-prefetch" href="https://user-images.githubusercontent.com/">



  <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/frameworks-521cbf980c80.css" media="all" rel="stylesheet" />
  <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/github-f75b395c95ee.css" media="all" rel="stylesheet" />
  
  
  <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/site-e1e1bc98a53e.css" media="all" rel="stylesheet" />
  

  <meta name="viewport" content="width=device-width">
  
  <title>SpaDES4Dummies/SpaDES4Dummies.Rmd at master · CeresBarros/SpaDES4Dummies · GitHub</title>
  <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub">
  <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub">
  <meta property="fb:app_id" content="1401488693436528">

    
    <meta content="https://avatars0.githubusercontent.com/u/22214224?s=400&amp;v=4" property="og:image" /><meta content="GitHub" property="og:site_name" /><meta content="object" property="og:type" /><meta content="CeresBarros/SpaDES4Dummies" property="og:title" /><meta content="https://github.com/CeresBarros/SpaDES4Dummies" property="og:url" /><meta content="SpaDES4Dummies - A SpaDES crash-course" property="og:description" />

  <link rel="assets" href="https://assets-cdn.github.com/">
  
  <meta name="pjax-timeout" content="1000">
  
  <meta name="request-id" content="EDD9:2F2B2:B654B1:1365CF7:5A70D0FE" data-pjax-transient>
  

  <meta name="selected-link" value="repo_source" data-pjax-transient>

    <meta name="google-site-verification" content="KT5gs8h0wvaagLKAVWq8bbeNwnZZK1r1XQysX3xurLU">
  <meta name="google-site-verification" content="ZzhVyEFwb7w3e0-uOTltm8Jsck2F5StVihD0exw2fsA">
  <meta name="google-site-verification" content="GXs5KoUUkNCoaAZn7wPN-t01Pywp9M3sEjnt_3_ZWPc">
    <meta name="google-analytics" content="UA-3769691-2">

<meta content="collector.githubapp.com" name="octolytics-host" /><meta content="github" name="octolytics-app-id" /><meta content="https://collector.githubapp.com/github-external/browser_event" name="octolytics-event-url" /><meta content="EDD9:2F2B2:B654B1:1365CF7:5A70D0FE" name="octolytics-dimension-request_id" /><meta content="iad" name="octolytics-dimension-region_edge" /><meta content="iad" name="octolytics-dimension-region_render" />
<meta content="https://github.com/hydro_browser_events" name="hydro-events-url" />
<meta content="/&lt;user-name&gt;/&lt;repo-name&gt;/blob/show" data-pjax-transient="true" name="analytics-location" />




  <meta class="js-ga-set" name="dimension1" content="Logged Out">


  

      <meta name="hostname" content="github.com">
  <meta name="user-login" content="">

      <meta name="expected-hostname" content="github.com">
    <meta name="js-proxy-site-detection-payload" content="ZmU0MmU0ZTdkMDhmM2YxOTliNDg2Y2NhM2NmOGZiNDUxMGFhOGQ4ZGM2YzQ4NTQ4Mzg4OWE2NjIyZmFiOTBiNXx7InJlbW90ZV9hZGRyZXNzIjoiMTMyLjE1Ni4xNDguMjEyIiwicmVxdWVzdF9pZCI6IkVERDk6MkYyQjI6QjY1NEIxOjEzNjVDRjc6NUE3MEQwRkUiLCJ0aW1lc3RhbXAiOjE1MTczNDI5NzUsImhvc3QiOiJnaXRodWIuY29tIn0=">

    <meta name="enabled-features" content="UNIVERSE_BANNER,FREE_TRIALS,MARKETPLACE_HERO_CARD_UPLOADER">

  <meta name="html-safe-nonce" content="368a2a90e1b3b0f9b880b93e8efa1917dba6afa5">

  <meta http-equiv="x-pjax-version" content="2ec2e5bba9c4872d5bfe40441b87cd01">
  

      <link href="https://github.com/CeresBarros/SpaDES4Dummies/commits/master.atom" rel="alternate" title="Recent Commits to SpaDES4Dummies:master" type="application/atom+xml">

  <meta name="description" content="SpaDES4Dummies - A SpaDES crash-course">
  <meta name="go-import" content="github.com/CeresBarros/SpaDES4Dummies git https://github.com/CeresBarros/SpaDES4Dummies.git">

  <meta content="22214224" name="octolytics-dimension-user_id" /><meta content="CeresBarros" name="octolytics-dimension-user_login" /><meta content="112410440" name="octolytics-dimension-repository_id" /><meta content="CeresBarros/SpaDES4Dummies" name="octolytics-dimension-repository_nwo" /><meta content="true" name="octolytics-dimension-repository_public" /><meta content="false" name="octolytics-dimension-repository_is_fork" /><meta content="112410440" name="octolytics-dimension-repository_network_root_id" /><meta content="CeresBarros/SpaDES4Dummies" name="octolytics-dimension-repository_network_root_nwo" /><meta content="false" name="octolytics-dimension-repository_explore_github_marketplace_ci_cta_shown" />


    <link rel="canonical" href="https://github.com/CeresBarros/SpaDES4Dummies/blob/master/SpaDES4Dummies.Rmd" data-pjax-transient>


  <meta name="browser-stats-url" content="https://api.github.com/_private/browser/stats">

  <meta name="browser-errors-url" content="https://api.github.com/_private/browser/errors">

  <link rel="mask-icon" href="https://assets-cdn.github.com/pinned-octocat.svg" color="#000000">
  <link rel="icon" type="image/x-icon" class="js-site-favicon" href="https://assets-cdn.github.com/favicon.ico">

<meta name="theme-color" content="#1e2327">



  </head>

  <body class="logged-out env-production page-blob">
    

  <div class="position-relative js-header-wrapper ">
    <a href="#start-of-content" tabindex="1" class="px-2 py-4 show-on-focus js-skip-to-content">Skip to content</a>
    <div id="js-pjax-loader-bar" class="pjax-loader-bar"><div class="progress"></div></div>

    
    
    



        <header class="Header header-logged-out  position-relative f4 py-3" role="banner">
  <div class="container-lg d-flex px-3">
    <div class="d-flex flex-justify-between flex-items-center">
      <a class="header-logo-invertocat my-0" href="https://github.com/" aria-label="Homepage" data-ga-click="(Logged out) Header, go to homepage, icon:logo-wordmark">
        <svg aria-hidden="true" class="octicon octicon-mark-github" height="32" version="1.1" viewBox="0 0 16 16" width="32"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z"/></svg>
      </a>

    </div>

    <div class="HeaderMenu HeaderMenu--bright d-flex flex-justify-between flex-auto">
        <nav class="mt-0">
          <ul class="d-flex list-style-none">
              <li class="ml-2">
                <a href="/features" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:features" data-selected-links="/features /features/project-management /features/code-review /features/project-management /features/integrations /features">
                  Features
</a>              </li>
              <li class="ml-4">
                <a href="/business" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:business" data-selected-links="/business /business/security /business/customers /business">
                  Business
</a>              </li>

              <li class="ml-4">
                <a href="/explore" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:explore" data-selected-links="/explore /trending /trending/developers /integrations /integrations/feature/code /integrations/feature/collaborate /integrations/feature/ship showcases showcases_search showcases_landing /explore">
                  Explore
</a>              </li>

              <li class="ml-4">
                    <a href="/marketplace" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:marketplace" data-selected-links=" /marketplace">
                      Marketplace
</a>              </li>
              <li class="ml-4">
                <a href="/pricing" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:pricing" data-selected-links="/pricing /pricing/developer /pricing/team /pricing/business-hosted /pricing/business-enterprise /pricing">
                  Pricing
</a>              </li>
          </ul>
        </nav>

      <div class="d-flex">
          <div class="d-lg-flex flex-items-center mr-3">
            <div class="header-search scoped-search site-scoped-search js-site-search" role="search">
  <!-- '"` --><!-- </textarea></xmp> --></option></form><form accept-charset="UTF-8" action="/CeresBarros/SpaDES4Dummies/search" class="js-site-search-form" data-scoped-search-url="/CeresBarros/SpaDES4Dummies/search" data-unscoped-search-url="/search" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
    <label class="form-control header-search-wrapper js-chromeless-input-container">
        <a href="/CeresBarros/SpaDES4Dummies/blob/master/SpaDES4Dummies.Rmd" class="header-search-scope no-underline">This repository</a>
      <input type="text"
        class="form-control header-search-input js-site-search-focus js-site-search-field is-clearable"
        data-hotkey="s"
        name="q"
        value=""
        placeholder="Search"
        aria-label="Search this repository"
        data-unscoped-placeholder="Search GitHub"
        data-scoped-placeholder="Search"
        autocapitalize="off">
        <input type="hidden" class="js-site-search-type-field" name="type" >
    </label>
</form></div>

          </div>

        <span class="d-inline-block">
            <div class="HeaderNavlink px-0 py-2 m-0">
              <a class="text-bold text-white no-underline" href="/login?return_to=%2FCeresBarros%2FSpaDES4Dummies%2Fblob%2Fmaster%2FSpaDES4Dummies.Rmd" data-ga-click="(Logged out) Header, clicked Sign in, text:sign-in">Sign in</a>
                <span class="text-gray">or</span>
                <a class="text-bold text-white no-underline" href="/join?source=header-repo" data-ga-click="(Logged out) Header, clicked Sign up, text:sign-up">Sign up</a>
            </div>
        </span>
      </div>
    </div>
  </div>
</header>

  </div>

  <div id="start-of-content" class="show-on-focus"></div>

    <div id="js-flash-container">
</div>



  <div role="main" class="application-main ">
        <div itemscope itemtype="http://schema.org/SoftwareSourceCode" class="">
    <div id="js-repo-pjax-container" data-pjax-container >
      






  <div class="pagehead repohead instapaper_ignore readability-menu experiment-repo-nav  ">
    <div class="repohead-details-container clearfix container">

      <ul class="pagehead-actions">
  <li>
      <a href="/login?return_to=%2FCeresBarros%2FSpaDES4Dummies"
    class="btn btn-sm btn-with-count tooltipped tooltipped-n"
    aria-label="You must be signed in to watch a repository" rel="nofollow">
    <svg aria-hidden="true" class="octicon octicon-eye" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M8.06 2C3 2 0 8 0 8s3 6 8.06 6C13 14 16 8 16 8s-3-6-7.94-6zM8 12c-2.2 0-4-1.78-4-4 0-2.2 1.8-4 4-4 2.22 0 4 1.8 4 4 0 2.22-1.78 4-4 4zm2-4c0 1.11-.89 2-2 2-1.11 0-2-.89-2-2 0-1.11.89-2 2-2 1.11 0 2 .89 2 2z"/></svg>
    Watch
  </a>
  <a class="social-count" href="/CeresBarros/SpaDES4Dummies/watchers"
     aria-label="0 users are watching this repository">
    0
  </a>

  </li>

  <li>
      <a href="/login?return_to=%2FCeresBarros%2FSpaDES4Dummies"
    class="btn btn-sm btn-with-count tooltipped tooltipped-n"
    aria-label="You must be signed in to star a repository" rel="nofollow">
    <svg aria-hidden="true" class="octicon octicon-star" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M14 6l-4.9-.64L7 1 4.9 5.36 0 6l3.6 3.26L2.67 14 7 11.67 11.33 14l-.93-4.74z"/></svg>
    Star
  </a>

    <a class="social-count js-social-count" href="/CeresBarros/SpaDES4Dummies/stargazers"
      aria-label="0 users starred this repository">
      0
    </a>

  </li>

  <li>
      <a href="/login?return_to=%2FCeresBarros%2FSpaDES4Dummies"
        class="btn btn-sm btn-with-count tooltipped tooltipped-n"
        aria-label="You must be signed in to fork a repository" rel="nofollow">
        <svg aria-hidden="true" class="octicon octicon-repo-forked" height="16" version="1.1" viewBox="0 0 10 16" width="10"><path fill-rule="evenodd" d="M8 1a1.993 1.993 0 0 0-1 3.72V6L5 8 3 6V4.72A1.993 1.993 0 0 0 2 1a1.993 1.993 0 0 0-1 3.72V6.5l3 3v1.78A1.993 1.993 0 0 0 5 15a1.993 1.993 0 0 0 1-3.72V9.5l3-3V4.72A1.993 1.993 0 0 0 8 1zM2 4.2C1.34 4.2.8 3.65.8 3c0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zm3 10c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zm3-10c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2z"/></svg>
        Fork
      </a>

    <a href="/CeresBarros/SpaDES4Dummies/network" class="social-count"
       aria-label="2 users forked this repository">
      2
    </a>
  </li>
</ul>

      <h1 class="public ">
  <svg aria-hidden="true" class="octicon octicon-repo" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M4 9H3V8h1v1zm0-3H3v1h1V6zm0-2H3v1h1V4zm0-2H3v1h1V2zm8-1v12c0 .55-.45 1-1 1H6v2l-1.5-1.5L3 16v-2H1c-.55 0-1-.45-1-1V1c0-.55.45-1 1-1h10c.55 0 1 .45 1 1zm-1 10H1v2h2v-1h3v1h5v-2zm0-10H2v9h9V1z"/></svg>
  <span class="author" itemprop="author"><a href="/CeresBarros" class="url fn" rel="author">CeresBarros</a></span><!--
--><span class="path-divider">/</span><!--
--><strong itemprop="name"><a href="/CeresBarros/SpaDES4Dummies" data-pjax="#js-repo-pjax-container">SpaDES4Dummies</a></strong>

</h1>

    </div>
    
<nav class="reponav js-repo-nav js-sidenav-container-pjax container"
     itemscope
     itemtype="http://schema.org/BreadcrumbList"
     role="navigation"
     data-pjax="#js-repo-pjax-container">

  <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
    <a href="/CeresBarros/SpaDES4Dummies" class="js-selected-navigation-item selected reponav-item" data-hotkey="g c" data-selected-links="repo_source repo_downloads repo_commits repo_releases repo_tags repo_branches repo_packages /CeresBarros/SpaDES4Dummies" itemprop="url">
      <svg aria-hidden="true" class="octicon octicon-code" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M9.5 3L8 4.5 11.5 8 8 11.5 9.5 13 14 8 9.5 3zm-5 0L0 8l4.5 5L6 11.5 2.5 8 6 4.5 4.5 3z"/></svg>
      <span itemprop="name">Code</span>
      <meta itemprop="position" content="1">
</a>  </span>

    <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
      <a href="/CeresBarros/SpaDES4Dummies/issues" class="js-selected-navigation-item reponav-item" data-hotkey="g i" data-selected-links="repo_issues repo_labels repo_milestones /CeresBarros/SpaDES4Dummies/issues" itemprop="url">
        <svg aria-hidden="true" class="octicon octicon-issue-opened" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M7 2.3c3.14 0 5.7 2.56 5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 0 1 1.3 8c0-3.14 2.56-5.7 5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14 7-7-3.14-7-7-7zm1 3H6v5h2V4zm0 6H6v2h2v-2z"/></svg>
        <span itemprop="name">Issues</span>
        <span class="Counter">0</span>
        <meta itemprop="position" content="2">
</a>    </span>

  <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
    <a href="/CeresBarros/SpaDES4Dummies/pulls" class="js-selected-navigation-item reponav-item" data-hotkey="g p" data-selected-links="repo_pulls /CeresBarros/SpaDES4Dummies/pulls" itemprop="url">
      <svg aria-hidden="true" class="octicon octicon-git-pull-request" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M11 11.28V5c-.03-.78-.34-1.47-.94-2.06C9.46 2.35 8.78 2.03 8 2H7V0L4 3l3 3V4h1c.27.02.48.11.69.31.21.2.3.42.31.69v6.28A1.993 1.993 0 0 0 10 15a1.993 1.993 0 0 0 1-3.72zm-1 2.92c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zM4 3c0-1.11-.89-2-2-2a1.993 1.993 0 0 0-1 3.72v6.56A1.993 1.993 0 0 0 2 15a1.993 1.993 0 0 0 1-3.72V4.72c.59-.34 1-.98 1-1.72zm-.8 10c0 .66-.55 1.2-1.2 1.2-.65 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2zM2 4.2C1.34 4.2.8 3.65.8 3c0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2z"/></svg>
      <span itemprop="name">Pull requests</span>
      <span class="Counter">0</span>
      <meta itemprop="position" content="3">
</a>  </span>

    <a href="/CeresBarros/SpaDES4Dummies/projects" class="js-selected-navigation-item reponav-item" data-hotkey="g b" data-selected-links="repo_projects new_repo_project repo_project /CeresBarros/SpaDES4Dummies/projects">
      <svg aria-hidden="true" class="octicon octicon-project" height="16" version="1.1" viewBox="0 0 15 16" width="15"><path fill-rule="evenodd" d="M10 12h3V2h-3v10zm-4-2h3V2H6v8zm-4 4h3V2H2v12zm-1 1h13V1H1v14zM14 0H1a1 1 0 0 0-1 1v14a1 1 0 0 0 1 1h13a1 1 0 0 0 1-1V1a1 1 0 0 0-1-1z"/></svg>
      Projects
      <span class="Counter" >0</span>
</a>


  <a href="/CeresBarros/SpaDES4Dummies/pulse" class="js-selected-navigation-item reponav-item" data-selected-links="repo_graphs repo_contributors dependency_graph pulse /CeresBarros/SpaDES4Dummies/pulse">
    <svg aria-hidden="true" class="octicon octicon-graph" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M16 14v1H0V0h1v14h15zM5 13H3V8h2v5zm4 0H7V3h2v10zm4 0h-2V6h2v7z"/></svg>
    Insights
</a>

</nav>


  </div>

<div class="container new-discussion-timeline experiment-repo-nav  ">
  <div class="repository-content ">

    
  <a href="/CeresBarros/SpaDES4Dummies/blob/a86b723c9c7d07860ed8b931bcfb5d3e6d051498/SpaDES4Dummies.Rmd" class="d-none js-permalink-shortcut" data-hotkey="y">Permalink</a>

  <!-- blob contrib key: blob_contributors:v21:16b54ce4075d9680153c157ef80fb1ae -->

  <div class="file-navigation js-zeroclipboard-container">
    
<div class="select-menu branch-select-menu js-menu-container js-select-menu float-left">
  <button class=" btn btn-sm select-menu-button js-menu-target css-truncate" data-hotkey="w"
    
    type="button" aria-label="Switch branches or tags" aria-expanded="false" aria-haspopup="true">
      <i>Branch:</i>
      <span class="js-select-button css-truncate-target">master</span>
  </button>

  <div class="select-menu-modal-holder js-menu-content js-navigation-container" data-pjax>

    <div class="select-menu-modal">
      <div class="select-menu-header">
        <svg aria-label="Close" class="octicon octicon-x js-menu-close" height="16" role="img" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M7.48 8l3.75 3.75-1.48 1.48L6 9.48l-3.75 3.75-1.48-1.48L4.52 8 .77 4.25l1.48-1.48L6 6.52l3.75-3.75 1.48 1.48z"/></svg>
        <span class="select-menu-title">Switch branches/tags</span>
      </div>

      <div class="select-menu-filters">
        <div class="select-menu-text-filter">
          <input type="text" aria-label="Filter branches/tags" id="context-commitish-filter-field" class="form-control js-filterable-field js-navigation-enable" placeholder="Filter branches/tags">
        </div>
        <div class="select-menu-tabs">
          <ul>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="branches" data-filter-placeholder="Filter branches/tags" class="js-select-menu-tab" role="tab">Branches</a>
            </li>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="tags" data-filter-placeholder="Find a tag…" class="js-select-menu-tab" role="tab">Tags</a>
            </li>
          </ul>
        </div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="branches" role="menu">

        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/CeresBarros/SpaDES4Dummies/blob/development/SpaDES4Dummies.Rmd"
               data-name="development"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                development
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open selected"
               href="/CeresBarros/SpaDES4Dummies/blob/master/SpaDES4Dummies.Rmd"
               data-name="master"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                master
              </span>
            </a>
        </div>

          <div class="select-menu-no-results">Nothing to show</div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="tags">
        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


        </div>

        <div class="select-menu-no-results">Nothing to show</div>
      </div>

    </div>
  </div>
</div>

    <div class="BtnGroup float-right">
      <a href="/CeresBarros/SpaDES4Dummies/find/master"
            class="js-pjax-capture-input btn btn-sm BtnGroup-item"
            data-pjax
            data-hotkey="t">
        Find file
      </a>
      <button aria-label="Copy file path to clipboard" class="js-zeroclipboard btn btn-sm BtnGroup-item tooltipped tooltipped-s" data-copied-hint="Copied!" type="button">Copy path</button>
    </div>
    <div class="breadcrumb js-zeroclipboard-target">
      <span class="repo-root js-repo-root"><span class="js-path-segment"><a href="/CeresBarros/SpaDES4Dummies" data-pjax="true"><span>SpaDES4Dummies</span></a></span></span><span class="separator">/</span><strong class="final-path">SpaDES4Dummies.Rmd</strong>
    </div>
  </div>


  <include-fragment class="commit-tease" src="/CeresBarros/SpaDES4Dummies/contributors/master/SpaDES4Dummies.Rmd">
    <div>
      Fetching contributors&hellip;
    </div>

    <div class="commit-tease-contributors">
      <img alt="" class="loader-loading float-left" height="16" src="https://assets-cdn.github.com/images/spinners/octocat-spinner-32-EAF2F5.gif" width="16" />
      <span class="loader-error">Cannot retrieve contributors at this time</span>
    </div>
</include-fragment>

  <div class="file">
    <div class="file-header">
  <div class="file-actions">

    <div class="BtnGroup">
      <a href="/CeresBarros/SpaDES4Dummies/raw/master/SpaDES4Dummies.Rmd" class="btn btn-sm BtnGroup-item" id="raw-url">Raw</a>
        <a href="/CeresBarros/SpaDES4Dummies/blame/master/SpaDES4Dummies.Rmd" class="btn btn-sm js-update-url-with-hash BtnGroup-item" data-hotkey="b">Blame</a>
      <a href="/CeresBarros/SpaDES4Dummies/commits/master/SpaDES4Dummies.Rmd" class="btn btn-sm BtnGroup-item" rel="nofollow">History</a>
    </div>


        <button type="button" class="btn-octicon disabled tooltipped tooltipped-nw"
          aria-label="You must be signed in to make or propose changes">
          <svg aria-hidden="true" class="octicon octicon-pencil" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M0 12v3h3l8-8-3-3-8 8zm3 2H1v-2h1v1h1v1zm10.3-9.3L12 6 9 3l1.3-1.3a.996.996 0 0 1 1.41 0l1.59 1.59c.39.39.39 1.02 0 1.41z"/></svg>
        </button>
        <button type="button" class="btn-octicon btn-octicon-danger disabled tooltipped tooltipped-nw"
          aria-label="You must be signed in to make or propose changes">
          <svg aria-hidden="true" class="octicon octicon-trashcan" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M11 2H9c0-.55-.45-1-1-1H5c-.55 0-1 .45-1 1H2c-.55 0-1 .45-1 1v1c0 .55.45 1 1 1v9c0 .55.45 1 1 1h7c.55 0 1-.45 1-1V5c.55 0 1-.45 1-1V3c0-.55-.45-1-1-1zm-1 12H3V5h1v8h1V5h1v8h1V5h1v8h1V5h1v9zm1-10H2V3h9v1z"/></svg>
        </button>
  </div>

  <div class="file-info">
      609 lines (468 sloc)
      <span class="file-info-divider"></span>
    33.2 KB
  </div>
</div>

    
  <div id="readme" class="readme blob instapaper_body">
    <article class="markdown-body entry-content" itemprop="text"><table data-table-type="yaml-metadata">
  <thead>
  <tr>
  <th>title</th>
  <th>author</th>
  <th>date</th>
  <th>output</th>
  </tr>
  </thead>
  <tbody>
  <tr>
  <td><div>SpaDES 4 Dummies</div></td>
  <td><div>Ceres Barros, Tati Micheletti</div></td>
  <td><div>Updated December 14, 2017</div></td>
  <td><div>html_document</div></td>
  </tr>
  </tbody>
</table>

<h2><a href="#a-very-simple-example-of-spades-awesomeness" aria-hidden="true" class="anchor" id="user-content-a-very-simple-example-of-spades-awesomeness"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>a VERY simple example of <strong><code>SpaDES</code></strong> awesomeness</h2>
<p>This guide will take you through how to start your own model using <strong><code>SpaDES</code></strong>. It assumes you have already installed <strong><code>SpaDES</code></strong> and that it is working in your machine. If you haven't done this yet, please have a look at <a href="https://gist.github.com/tati-micheletti/1de7505cdd387fe997f127f13eeb4393">this gist</a> to bypass known issues.</p>
<h3><a href="#the-example" aria-hidden="true" class="anchor" id="user-content-the-example"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>THE EXAMPLE</h3>
<p>Let's imagine we want to understand whether the abundance of a species is statistically related with temperature. Both the abundance data and the temperature data are being constantly updated. Also, we want to have the possibility of analysing the relationship between the two iteratively, without needing to constantly change our script to account for the new data inputs.
In this example the abundance and temperature data are being updated by a simulation model.</p>
<h3><a href="#before-spades" aria-hidden="true" class="anchor" id="user-content-before-spades"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>BEFORE <code>SpaDES</code>...</h3>
<p>If we use R to develop our species abundance and temperature simulation models in the 'conventional way', we'll probably have a script where everything happens - the simulations and data analysis. At most, we would have a main script that sources others that contain useful functions, or are doing the simulations and data treatment/analysis separately. If you already use R like this, you'll find that the <strong><code>SpaDES</code></strong> way of thinking is similar. So why use it? Because it makes changing, adapting and sharing your code - or modules - much easier.</p>
<p>Still don't believe me? Okay, let's solve our problem using the conventional way:</p>
<h4><a href="#setup" aria-hidden="true" class="anchor" id="user-content-setup"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>SETUP</h4>
<p>Create a raster template:</p>
<pre lang="{r"><code>library(raster)

r &lt;- raster(nrows = 100, ncols = 100, xmn = -50, xmx = 50, ymn = -50, ymx = 50)
</code></pre>
<h4><a href="#species-abundance-simulations" aria-hidden="true" class="anchor" id="user-content-species-abundance-simulations"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>SPECIES ABUNDANCE "SIMULATIONS"</h4>
<p>Our VERY simple "simulation" model (in form of a function) generates rasters that follow a Gaussian distribution</p>
<pre lang="{r"><code>abundance_model &lt;- function(r, Time) {
  abund_outputs &lt;- list()
  for(t in 1:Time) { 
    abund_outputs[[t]] &lt;- SpaDES.tools::gaussMap(r, scale = 100, var = 0.03) 
  }
  return(abund_outputs)
}
</code></pre>
<p>Set the length of the simulation (or simply the number of model iterations), run it and plot results (all ABUNDANCE plots together):</p>
<pre lang="{r"><code>Time &lt;- 10
abundance &lt;- abundance_model(r = r, Time = Time)
plot(stack(abundance))
</code></pre>
<h4><a href="#temperature-simulations" aria-hidden="true" class="anchor" id="user-content-temperature-simulations"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>TEMPERATURE "SIMULATIONS"</h4>
<p>The temperature simulation model will be similar to the vegetation one - remember this is a dummy example ;)</p>
<pre lang="{r"><code>temp_model &lt;- function(r, Time) {
  temp_outputs &lt;- list()
  for(t in 1:Time) { 
    temp_outputs[[t]] &lt;- SpaDES.tools::gaussMap(r, scale = 100, var = 0.1) 
  }
  return(temp_outputs)
}
</code></pre>
<p>Run the model and plot results (all TEMPERATURE plots together)</p>
<pre lang="{r"><code>temperature &lt;- temp_model(r = r, Time = Time)
plot(stack(temperature))
</code></pre>
<h4><a href="#data-analysis" aria-hidden="true" class="anchor" id="user-content-data-analysis"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>DATA ANALYSIS</h4>
<p>Now we analyse if species abundance and temperature are correlated.<br>
First we create the data analysis function (a simple linear model):</p>
<pre lang="{r"><code>stats_analysis &lt;- function(Data){
  lm1 &lt;- lm(abund ~ temp, data = Data)
  plot(Data$abund ~ Data$temp, xlab = "Temperature", ylab = "Species abundance")
  abline(a = lm1$coefficients["(Intercept)"], b = lm1$coefficients["temp"], lwd = 2, col = "blue")
}
</code></pre>
<p>Then we create a loop to analyse each plot of our time-series:</p>
<pre lang="{r"><code>par(mfrow = c(2, 4)) # This plots in 2 lines and 4 columns
for(t in 1:Time){
  outputdata &lt;- data.frame(abund = abundance[[t]][], temp = temperature[[t]][])
  stats_analysis(Data = outputdata)
}
</code></pre>
<p>That's it. You have your model. But what if you need to include new data? You would have to manually change the code for it...</p>
<h3><a href="#after--spades" aria-hidden="true" class="anchor" id="user-content-after--spades"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>AFTER  <code>SpaDES</code>...</h3>
<p>Let's now solve the same problem using the <strong><code>SpaDES</code></strong> approach. I like to start by creating a <em>global.R</em> script (you can call it whatever you want, though) that will load the <strong><code>SpaDES</code></strong> libraries and run <strong><code>SpaDES</code></strong> simulations. The <em>global.R</em> script for this dummy example can be found on the root of SpaDES4Dummies Project.</p>
<pre lang="{r"><code>library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies

## Create and set paths inside the current working directory
setPaths(cachePath = "cache",
         inputPath = "inputs",
         modulePath = "modules",
         outputPath = "outputs")

# getPaths() ## allows you to retrieve the paths once they are created

## Let's create a self-contained module that will simulate the species' abundance for any given period of time and frequency.
if(!dir.exists(file.path(getPaths()$modulePath, "speciesAbundance"))){
  newModule(name = "speciesAbundance", path = getPaths()$modulePath)
}
</code></pre>
<p>You will notice that <code>newModule</code> has created a module folder (<code>speciesAbundance</code>) inside <em>/modules</em> that contains both the module <em>.R</em> script template, as well as the documentation template (the <em>.Rmd</em> file). Although we will not be discussing the <em>.Rmd</em> file, please bear in mind that this is a <strong>fundamental</strong> part of creating a reproducible and transparent module - check out the <a href="http://www.britishecologicalsociety.org/wp-content/uploads/2017/12/guide-to-reproducible-code.pdf" rel="nofollow">Guide to Reproducible Code in Ecology and Evolution</a> from the British Ecological Society). The documentation should contain not only the description of the module, but also some simple examples of what it can do.</p>
<p><code>newModule</code> also created the folder <em>/data</em> where data necessary to the module should be put in, and the folder <em>/tests</em> that may contain testing scripts. We won't be using either of them in this example.</p>
<p><strong>/!\ Attention /!\</strong> : <code>newModule</code> should only be run once, or separately in the console, otherwise you may lose your module edits by re-running it again - this is why I wrapped it in an <code>if</code> statement (smart eh?).</p>
<p>Now go ahead, open the <em>speciesAbundance.R</em> script and check it out a little bit.</p>
<hr>
<h4><a href="#species-abundance-speciesabundance-module" aria-hidden="true" class="anchor" id="user-content-species-abundance-speciesabundance-module"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>SPECIES ABUNDANCE (<code>speciesAbundance</code>) Module</h4>
<p>I know, I know... It seems confusing and there's a lot of stuff in that template! But I will describe it step by step. We'll go through it step by step (although not necessarily following the order of the script). The module script can be divided into 4 parts:</p>
<p><strong>1. <a href="#anchor1">Defining the Module</a>:</strong> this is where you <strong>define</strong> your module (or the module's metadata) (e.g. module author(s), time units, basic parameters, general inputs and outputs, etc.);<br>
<strong>2. <a href="#anchor2">Event functions</a>:</strong> these are the "actions" (or events) that will happen in your module (i.e. species reproduction, plotting, saving parameters) - simply put, <strong>WHAT</strong> the module will do;<br>
<strong>3. <a href="#anchor3">Scheduling Events</a>:</strong> this is how SpaDES schedules when each event is going to happen - in which order (e.g. during the simulation, when will SpaDES plot a graph) - simply put, <strong>WHEN</strong> the module will do;<br>
<strong>4. <a href="#anchor4">Additional functions</a>:</strong> any additional functions needed (e.g. this is used to keep the coding of your module as clear and straightforward as possible);</p>
<p>The first thing you need to know is that <strong>you won't need to run</strong> any of the code inside a module's <em>.R</em> script. The function <code>simInit</code> will "call" your model when it sets up the simulation. But don't worry about <code>simInit</code> yet, you will see this later in detail. So let's go through the module <em>.R</em> script together.</p>
<p>####<strong>1. Defining the Module</strong>{#anchor1}</p>
<p>The first "bit" of the code is basically defining the module's <a href="http://data-informed.com/what-is-metadata-a-simple-guide-to-what-everyone-should-know/" rel="nofollow">metadata</a>. It will allow you to define the module's author, keywords that describe the module, any required packages, and the module(s) and package(s) version(s). It will also define parameters and inputs that the module requires, and the outputs it produces.</p>
<p>This dummy module example requires no input data, as the data is generated by a function in the code (remember the function <code>abundance_model</code> from the "Before SpaDES..." section?), so we leave that bit empty. As for the outputs, it produces a template raster (produced during the <code>abundanceInit</code> event) and a list of abundance rasters (produced during the <code>abundanceSim</code> event). So we define these two outputs in the function.</p>
<p>If you compare this dummy module to a template created by the <code>newModule</code> function, you'll notice that several parameters that we don't need for our dummy example were removed (like <code>save parameters</code>).</p>
<p>If you are unsure of what input and output parameters are in the context of a module, a good rule of thumb is that inputs are all the <code>sim$...</code> objects that appear to the <strong>right-hand side</strong> of a <code>&lt;-</code>, whereas output parameters are the <code>sim$...</code> objects appear to the <strong>left-hand side</strong> of a <code>&lt;-</code>. Another way of explaining it for objects ("obj"):</p>
<p><a href="/CeresBarros/SpaDES4Dummies/blob/master/obj.png" target="_blank"><img src="/CeresBarros/SpaDES4Dummies/raw/master/obj.png" alt="Inputs and outputs in SpaDES: Object A comes from outside of the module (e.g. from an internet URL, from data you have, etc), while Module Z produces object C. Both objects serve as an inputs for Module Y, which in return produce as outputs objects B and D, respectivelly from objects A and C. As Module Z uses a simple function &quot;internally&quot; to create object C, it doesn't have any inputs, such as our dummy example." style="max-width:100%;"></a></p>
<p>Here's how I defined the module:</p>
<pre lang="{r"><code>defineModule(sim, list(
  name = "speciesAbundance",
  description = "Species abundance simulator",
  keywords = c("species", "abundance", "gaussian", "spatial"),
  authors = person("Mr.", "Me", email = "mr.me@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0", speciesAbundance = "0.0.1", SpaDES.addins = "0.1.0", SpaDES.tools = "0.1.0", raster = "2.6-7"),
  # spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "speciesAbundance.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("simulationTimeStep", "numeric", 1, NA, NA, "This describes the simulation time step interval"),
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time interval between plot events")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "r", objectClass = "RasterLayer", desc = "Template raster"),
    createsOutput("abundRasters", "RasterLayer", "Raster layer of species abundance at any given year")
  )
))

</code></pre>
<p>The rest of the script defines the events and their sequences for this module - remember <strong><code>SpaDES</code></strong> = Spatial Discrete Event Simulator - and the events themselves.</p>
<p>####<strong>2. Event functions</strong>{#anchor2}</p>
<p>Since we are only interested in simulating and plotting species abundances, we can remove unnecessary events from the script. Only 3 events were kept: the initialisation, an abundance simulation event and a plotting event.</p>
<p>Let's look at the <strong>event functions</strong> (<code>doEvent.moduleName</code>, in our dummy example, <code>doEvent.speciesAbundance</code> function) first. The event functions were renamed from the template as to reflect my module's actions. Therefore, <code>Init</code> became <code>abundanceInit</code>, <code>Plot</code> became <code>abundancePlot</code> and <code>event1</code> became <code>abundanceSim</code>.</p>
<p>#####<em>Initialisation function</em></p>
<p>The initialisation (<code>init</code>) event can be seen as the starting point of the module, and is <strong>mandatory</strong>. The other <code>events</code> are not. <strong><code>SpaDES</code></strong> knows how to deal with an <code>init</code> event and "searches" for it before doing anything else.</p>
<p>In the initialisation of our dummy example, we are creating a template raster and a storage list for our species abundance outputs (which will also be rasters). Notice how the only argument to <code>abundanceInit</code> is the <code>sim</code> object, which is where the objects being created are stored. Always take care to <code>return()</code> the <code>sim</code> object at the end of an event.</p>
<pre lang="{r"><code>abundanceInit &lt;- function(sim) {
  ## Template raster
  sim$r &lt;- raster(nrows = 100, ncols = 100, xmn = -50, xmx = 50, ymn = -50, ymx = 50)
  
  ## create storage list of species abundance
  sim$abundRasters &lt;- list()
  
  return(invisible(sim))
}
</code></pre>
<p>#####<em>Abundance simulation event function</em></p>
<p>This event is basically the 'prima donna' of this module. This is where we will generate species abundances. Notice how instead of a loop, we now have the event running the <code>abundance_model</code> function (which we'll define separately below) and storing its outputs in the <code>sim$abundRaster</code> object. Notice as well that we use <code>time(sim)</code> as the identifier of the list slots where outputs are stored. Go ahead and check what <code>SpaDES.core::time</code> is doing!
Again, the sole argument and output to this event function is the <code>sim</code> object - notice a pattern? ;)</p>
<pre lang="{r"><code>abundanceSim &lt;- function(sim) {
  ## Generate species abundances - our "simulation"
  sim$abundRasters[[time(sim)]] &lt;- abundance_model(ras = sim$r)
  
  return(invisible(sim))
}
</code></pre>
<p>#####<em>Plotting event function</em></p>
<p>What does this event do? Yup, you got it. This event simply plots the species abundance rasters that are produced during the <code>abundanceSim</code> event. Shall we have a look?</p>
<pre lang="{r"><code>abundancePlot &lt;- function(sim) {
  ## plot abundances
  plot(sim$abundRasters[[time(sim)]], 
       main = paste0("Species abundance\nat time ", time(sim)))
  
  return(invisible(sim))
}
</code></pre>
<p>"It looks great, but... How does it know that it should plot them <strong>after</strong> the abundance generator?" you ask, puzzled. Good question sir/madam, 5 points for Griffindor!</p>
<p>####<strong>3.Scheduling events</strong>{#anchor3}</p>
<p>The order in which each of the <em>modules</em> (and consequently their initialisations) are executed is automatically determined by inter-module dependencies (i.e. module inputs that are the outputs of other modules). After all, we don't want the plotting to occur before the generation of species abundances. If there are no inter-module dependencies the order is determined by the other in which the modules are listed in the <em>global.R</em> script <strong>and/or</strong> by the way we schedule the events - I told you <strong><code>SpaDES</code></strong> is cool!</p>
<p>So let's go back to our dummy example. Now that we've defined our event functions, we need to set the order in which they will happen for this module. For example, we don't want the plotting to occur before the generation of species abundances, so we'll schedule all <code>abundancePlot</code> events to occur slightly after the <code>abundanceSim</code> events:</p>
<pre lang="{r"><code>doEvent.speciesAbundance = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim &lt;- abundanceInit(sim)
      
      ## schedule future event(s)
      sim &lt;- scheduleEvent(sim, eventTime = start(sim), moduleName = "speciesAbundance", eventType = "SimulAbund")
      sim &lt;- scheduleEvent(sim, eventTime = P(sim)$.plotInitialTime, moduleName = "speciesAbundance", eventType = "plot")
    },
    plot = {
      ## do stuff for this event
      sim &lt;- abundancePlot(sim)
      
      ## schedule future event(s)
      sim &lt;- scheduleEvent(sim, eventTime = time(sim) + P(sim)$.plotInterval, moduleName = "speciesAbundance", eventType = "plot")
    },
    SimulAbund = {
      ## do stuff for this event
      sim &lt;- abundanceSim(sim)
      
      ## schedule future event(s)
      sim &lt;- scheduleEvent(sim, eventTime = time(sim) + P(sim)$simulationTimeStep, moduleName = "speciesAbundance", eventType = "SimulAbund")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}
</code></pre>
<p>Now this one seems complicated... But it's not, trust me. Let's go step by step.</p>
<p>I encourage you to start checking what <code>base::switch</code> is doing on your own, but if you can't wait to know it, here's a short explanation: <code>base::switch</code> tells <strong>R</strong> that the behaviour of your function will change (or switch) depending on the value of <code>eventType</code>. So we need to define the behaviour of <code>doEvent</code> for all the event types we have in this module. In each of them we will execute the event functions and schedule the future events with <code>scheduleEvent</code>.</p>
<ul>
<li>
<h5><a href="#init" aria-hidden="true" class="anchor" id="user-content-init"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a><strong>Init</strong></h5>
</li>
</ul>
<p>The first event is, obviously, <code>init</code> - don´t change the name of this one.
In <code>init</code> we run the initialisation event and schedule the abundance simulation and plotting events. The <code>init</code> for the plotting events will be executed at the time defined by the <code>.plotInitialTime</code> parameter, which is stored in the <code>sim</code> object (and obtained using <code>SpaDES.core::P</code>). Notice that the <code>abundanceSim</code> event will be generated at <code>start(sim)</code> (i.e. at the starting time of the simulation), which basically is the same as running it during the initialisation.</p>
<ul>
<li>
<h5><a href="#plot" aria-hidden="true" class="anchor" id="user-content-plot"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a><strong>Plot</strong></h5>
</li>
</ul>
<p>The <code>abundancePlot</code> plotting event type is defined next.
Besides doing the obvious plotting, it <strong>schedules itself</strong> to occur at a frequency defined by the <code>.plotInterval</code> parameter - isn't that cool?</p>
<ul>
<li>
<h5><a href="#simulabund" aria-hidden="true" class="anchor" id="user-content-simulabund"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a><strong>SimulAbund</strong></h5>
</li>
</ul>
<p>Finally, we define the <code>SimulAbund</code> event during which the species' abundances will be generated using the <code>abundanceSim</code> function.
It is similar to the plotting event, with an important difference being that it uses the <code>simulationTimeStep</code> parameter to re-schedule itself.</p>
<p><strong>Note:</strong> Notice how I've scheduled future events to <code>time(sim) + P(sim)$simulationTimeStep</code> or <code>time(sim) + P(sim)$.plotInterval</code>. This way, future events will occur in the future, depending on the time step and plot interval defined in your global script.</p>
<p>####<strong>4. Additional functions</strong>{#anchor4}</p>
<p>Ok, just one more bit left. Events may also rely on other functions that can either be sourced from other scripts, or defined at the end of the module script. This is the case for the species abundances generator function, which I have coded at the end of my <em>speciesAbundance.R</em> script:</p>
<pre lang="{r"><code>abundance_model &lt;- function(ras) {
  abund_ras &lt;- SpaDES.tools::gaussMap(ras, scale = 100, var = 0.01) 
  return(abund_ras)
}
</code></pre>
<h4><a href="#creating-additional-modules-temperature-module" aria-hidden="true" class="anchor" id="user-content-creating-additional-modules-temperature-module"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Creating additional modules: TEMPERATURE module</h4>
<p>Now let's go ahead and repeat the previous steps to create a second module that will generate yearly temperatures.</p>
<p>Apart from changing the objects and functions names, I have also included the template raster <code>r</code> as an input object for the temperature module - remember that <code>r</code> is created during the <code>abundanceInit</code>. This avoids repeating the creation of the template raster.
This may not seem like a big deal in our example, but it can be if you're generating heavy objects, or relying on functions that take a while to run.</p>
<p>Here's how my final <em>temperature.R</em> script looks like:</p>
<pre lang="{r"><code>defineModule(sim, list(
  name = "temperature",
  description = "Temperature simulator",
  keywords = c("temperature", "gaussian", "spatial"),
  authors = person("Mr.", "Me", email = "mr.me@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0", speciesAbundance = "0.0.1", temperature = "0.0.1", SpaDES.addins = "0.1.0", SpaDES.tools = "0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "temperature.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("simulationTimeStep", "numeric", 1, NA, NA, "This describes the simulation time step interval"),
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time interval between plot events")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("r", "RasterLayer", "Template raster")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput("tempRasters",  "list", "List of raster layers of temperature at any given year")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.temperature = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim &lt;- temperatureInit(sim)
      
      ## schedule future event(s)
      sim &lt;- scheduleEvent(sim, eventTime = start(sim), moduleName = "temperature", eventType = "SimulTemp")
      sim &lt;- scheduleEvent(sim, eventTime = P(sim)$.plotInitialTime, moduleName = "temperature", eventType = "plot")
    },
    plot = {
      ## do stuff for this event
      sim &lt;- temperaturePlot(sim)
      
      ## schedule future event(s)
      sim &lt;- scheduleEvent(sim, eventTime = time(sim) + P(sim)$.plotInterval, moduleName = "temperature", eventType = "plot")
    },
    SimulTemp = {
      ## do stuff for this event
      sim &lt;- temperatureSim(sim)
      
      ## schedule future event(s)
      sim &lt;- scheduleEvent(sim, eventTime = time(sim)+ P(sim)$simulationTimeStep, moduleName = "temperature", eventType = "SimulTemp")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## This is the 'init' event:
temperatureInit &lt;- function(sim) {
  ## create storage list of species temperature
  sim$tempRasters &lt;- list()
  
  return(invisible(sim))
}

## This is the plotting event funciton
temperaturePlot &lt;- function(sim) {
  ## plot temperature
  plot(sim$tempRasters[[time(sim)]], 
       main = paste0("Temperature\nat time ", time(sim)))
  
  return(invisible(sim))
}

## This is the temperature simulation event function
temperatureSim &lt;- function(sim) {
  ## Generate temperature - our "updated data"
  sim$tempRasters[[time(sim)]] &lt;- temperature_model(ras = sim$r)
  
  return(invisible(sim))
}

## This is not an event, but a function that we define separately 
## and that contains our "simulation model"
temperature_model &lt;- function(ras) {
  temp_ras &lt;- SpaDES.tools::gaussMap(ras, scale = 100, var = 0.01) 
  return(temp_ras)
}
</code></pre>
<h4><a href="#modules-that-depend-on-other-modules" aria-hidden="true" class="anchor" id="user-content-modules-that-depend-on-other-modules"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Modules that depend on other modules</h4>
<p>Our third and last module will be used to run the statistical analysis at each year, after the abundances and temperatures are generated.
Hence, it'll depend on the outputs of the <code>speciesAbundance</code> and the <code>temperature</code> modules. We'll call it <code>speciesTempLM</code> (<strong>species</strong> and <strong>Temp</strong>erature <strong>L</strong>inear <strong>M</strong>odel).</p>
<p>The interest of keeping the statistical analysis in a separate module lies on the fact that it allows us to easily swap and compare different statistical models to analyse our data if we want to.</p>
<p>It also allows for greater flexibility when it comes to <strong>when</strong> the statistical model is supposed to run. For example, imagine that instead of every year, we want to fit it at every 5 years, using the previous 5 years of data. By having the statistical analysis contained in its own module, we don't need to fiddle with the other module scripts in order to make these changes.</p>
<p>Finally, I'd like to draw your attention to a few differences in this module's script before we see it:</p>
<ul>
<li>
<p>The <strong>frequency</strong> of the statistical analysis (and correspondent plots) will be determined by the parameter <code>statsTimestep</code>. This parameter also determines the number of data years to be used to fit the linear model. If <code>statsTimestep</code> = 5, then the statistical analysis will use the precedent 5 years of data including the year in which the event is running (a total of 6 years of data);</p>
</li>
<li>
<p>This module <strong>requires inputs</strong>. They are specified in <code>inputObjects</code> part of <code>defineModule</code> - notice how I've respected the names, classes and description of the objects that come from the <code>speciesAbundance</code> and the <code>temperature</code> modules;</p>
</li>
<li>
<p>This time, we have <strong>two additional functions</strong> at the end of the script: the function fitting the linear model and a plotting function.</p>
</li>
</ul>
<p>Here's the full module's script.
Notice how the future events where scheduled to <code>P(sim)$statsTimestep + 0.1</code>. This forces the statistic analyses to occur <strong>after</strong> the abundance and temperature rasters are ready.</p>
<pre lang="{r"><code># Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList.
defineModule(sim, list(
  name = "speciesTempLM",
  description = "Statistical analysis of species ~ temperature relationships using LM",
  keywords = c("linear model"),
  authors = person("Mr.", "Me", email = "mr.me@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0", speciesTempLM = "0.0.1", speciesAbundance = "0.0.1", temperature = "0.0.1", SpaDES.addins = "0.1.0", SpaDES.tools = "0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "speciesTempLM.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("statsTimestep", "numeric", 1, NA, NA, "This describes the how often the statitiscal analysis will be done")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput( "abundRasters", "list", "List of raster layers of species abundance at any given year"),
    expectsInput( "tempRasters", "list", "List of raster layers of temperature at any given year")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput("outputdata", "list", "List of dataframes containing species abundances and temperature values per pixel"),
    createsOutput( "outputLM", "list", "List of output yearly LMs (abundance ~ temperature)"),
    createsOutput( "yrs", "numeric", "Vector of years used for statistical analysis")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.speciesTempLM = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim &lt;- statsInit(sim)

      ## schedule future event(s)
      sim &lt;- scheduleEvent(sim, P(sim)$statsTimestep + 0.1, "speciesTempLM", "stats")
      sim &lt;- scheduleEvent(sim, P(sim)$statsTimestep + 0.1, "speciesTempLM", "plot")
    },
    plot = {
      ## do stuff for this event
      sim &lt;- statsPlot(sim)
      
      ## schedule future event(s)
      sim &lt;- scheduleEvent(sim, time(sim) + P(sim)$statsTimestep, "speciesTempLM", "plot")
    },
    stats = {
      ## do stuff for this event
      sim &lt;- statsAnalysis(sim)
      
      ## schedule future event(s)
      sim &lt;- scheduleEvent(sim, time(sim) + P(sim)$statsTimestep, "speciesTempLM", "stats")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## template initialisation
statsInit &lt;- function(sim) {
  ## create outputs storage lists
  sim$outputdata &lt;- list()
  sim$outputLM &lt;- list()
  
  return(invisible(sim))
}

## Plotting event
statsPlot &lt;- function(sim) {

  plotLMResults(Data = sim$outputdata[[time(sim)]], model = sim$outputLM[[time(sim)]])
  
  return(invisible(sim))
}

## Statistical analysis event
statsAnalysis &lt;- function(sim) {
  
  sim$yrs &lt;- seq(time(sim) - P(sim)$statsTimestep + 1, time(sim), 1)

  sim$outputdata[[time(sim)]] &lt;- do.call(rbind.data.frame, 
                                         lapply(sim$yrs, FUN = function(y){
                                           abundTemp &lt;- data.frame(abund = sim$abundRasters[[y]][], temp = sim$tempRasters[[y]][], year = y)          
                                           return(abundTemp)
                                         }))
  
  sim$outputLM[[time(sim)]] &lt;- linearModel(Data = sim$outputdata[[time(sim)]])
  
  return(invisible(sim))
}

## Other functions
linearModel &lt;- function(Data){
  return(lm1 &lt;- lm(abund ~ temp, data = Data))
}

plotLMResults &lt;- function(Data, model){
  plot(Data$abund ~ Data$temp, xlab = "Temp.", ylab = "Species abundance", 
       main = paste("From years", min(Data$year)-0.1, "to", max(Data$year)-0.1, sep = " "))
  abline(a = model$coefficients["(Intercept)"], b = model$coefficients["temp"], lwd = 2, col = "blue")
}
</code></pre>
<h3><a href="#lets-play" aria-hidden="true" class="anchor" id="user-content-lets-play"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Let's play!</h3>
<p>Ok, now that we have our modules ready we can set up the simulation. Let's go back to our <em>global.R</em> script.</p>
<p>The function <code>simInit</code> will take a bunch of parameter lists and will prepare a simulation object that can later be run by the <code>spades</code> function. Let's see what these lists consist of:</p>
<p>The first list, <code>modules</code>, contains modules we want to activate.</p>
<p>Then, <code>times</code> is a list containing the start and end times of the simulation and what time units we're working with - so it defines the length of the simulation. It's important that the start and ending times are defined in decimals, because <strong><code>SpaDES</code></strong> allows decomposing time units into smaller fractions - and we'll take advantage of this feature!</p>
<p><code>parameters</code> is a list of lists. It contains the values of parameters used by each modules, and well as "global" parameters used by all of them (<code>.globals</code>). Note that not all module parameters need to be defined in this way. In fact, a module can use a parameter that is listed inside the module <em>.R</em> script (and has a default value set there), but is never listed in its <code>simInit</code>. An example of this would be <code>.plotInterval</code> which is used and defined in the <code>speciesAbundance</code> and <code>temperature</code> modules. Conversely, <code>.plotInitialTime</code> is a parameter used and defined in these modules, but whose value we change when running <code>simInit</code>.</p>
<p>To make sure that plotting always occurs after the abundances are generated, I have changed <code>.plotInitialTime</code> to 1.5. Since <code>.plotInterval</code> remains unchanged (remember, that is its defined within the module scripts), all plots will occur at the "half" year (1.5, 2.5, 3.5, etc.)
Finally, <code>paths</code> contains the directory paths that we set earlier.</p>
<pre lang="{r"><code>## list the modules to use
modules &lt;- list("speciesAbundance", "temperature", "speciesTempLM")

## Set simulation and module parameters
times &lt;- list(start = 1.0, end = 10.1, timeunit = "year")
parameters &lt;- list(
  .globals = list(simulationTimeStep = 1, .plotInitialTime = 1.5),
  speciesTempLM = list(statsTimestep = 5)
)

## make a list of directory paths
paths &lt;- getPaths()

## Simulation setup
mySim &lt;- simInit(times = times, params = parameters, 
                modules = modules, paths =  paths)
</code></pre>
<p>####Have you set up your simulation correctly?</p>
<p>Before starting the simulations we should check if the modules were linked correctly.</p>
<p><strong>Module diagram</strong></p>
<p><code>moduleDiagram</code> is a useful function that shows module interdependencies as a network. The direction of the arrows indicates an output to input flow. You can see that an output from <code>speciesAbundance</code> (specifically our <code>r</code> raster) is an input for <code>temperature</code>. In a similar way, the outputs of the <code>speciesAbundance</code> and <code>temperature</code> modules are inputs to the <code>speciesTempLM</code> module.</p>
<pre lang="{r"><code>clearPlot()
moduleDiagram(mySim)
</code></pre>
<p><strong>Object diagram</strong></p>
<p><code>objectDiagram</code> provides yet another way of checking if the modules are linked correctly, by explicitly showing the objects that pass between modules.</p>
<pre lang="{r"><code>objectDiagram(mySim)
</code></pre>
<p>####Run <strong><code>SpaDES</code></strong>!</p>
<p>Ok, everything seems to be correct so far. Let's try to run the simulations.
I have used <code>debug = TRUE</code> so that <code>spades</code> prints the events as they are being executed. In case something fails, we'll know where it stopped.</p>
<pre lang="{r"><code>## run simulation
dev() # on Windows and Mac, this opens external device if using Rstudio, it is faster
clearPlot()
spades(mySim, debug = TRUE)
</code></pre>
<p>Try now playing around with other models, parameters and new modules to further explore all the <code>SpaDES</code> flexibility! The more complex your project gets, the easier it is to turn <em>on</em> and <em>off</em> different modules, run different statistical analysis, and include more data  when using <code>SpaDES</code> than before!</p>
 <h4><a href="#happy-spadesing" aria-hidden="true" class="anchor" id="user-content-happy-spadesing"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>**Happy SpaDESing!**</h4> 
<h3><a href="#additional-notes" aria-hidden="true" class="anchor" id="user-content-additional-notes"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Additional notes</h3>
<p><strong><code>SpaDES</code></strong> is an extremely powerful package, whose potential goes well beyond what has been discussed in this dummy example. If you don't feel so dummy any more and want to explore it further, have a look at the <a href="https://github.com/PredictiveEcology/SpaDES-modules/blob/master/modules/LCC2005/LCC2005.Rmd">LCC2005</a> and the <a href="https://htmlpreview.github.io/?https://github.com/PredictiveEcology/SpaDES-modules/blob/master/modules/wolfAlps/wolfAlps.html" rel="nofollow">wolfALps</a> module tutorials. Also, do go to the <a href="http://predictiveecology.org/" rel="nofollow"><strong><code>SpaDES</code></strong> webpage</a> to find further information about the platform, as well as upcoming workshops and publications.</p>
</article>
  </div>

  </div>

  <button type="button" data-facebox="#jump-to-line" data-facebox-class="linejump" data-hotkey="l" class="d-none">Jump to Line</button>
  <div id="jump-to-line" style="display:none">
    <!-- '"` --><!-- </textarea></xmp> --></option></form><form accept-charset="UTF-8" action="" class="js-jump-to-line-form" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
      <input class="form-control linejump-input js-jump-to-line-field" type="text" placeholder="Jump to line&hellip;" aria-label="Jump to line" autofocus>
      <button type="submit" class="btn">Go</button>
</form>  </div>


  </div>
  <div class="modal-backdrop js-touch-events"></div>
</div>

    </div>
  </div>

  </div>

      
<div class="footer container-lg px-3" role="contentinfo">
  <div class="position-relative d-flex flex-justify-between py-6 mt-6 f6 text-gray border-top border-gray-light ">
    <ul class="list-style-none d-flex flex-wrap ">
      <li class="mr-3">&copy; 2018 <span title="0.09523s from unicorn-4151094294-93vld">GitHub</span>, Inc.</li>
        <li class="mr-3"><a href="https://github.com/site/terms" data-ga-click="Footer, go to terms, text:terms">Terms</a></li>
        <li class="mr-3"><a href="https://github.com/site/privacy" data-ga-click="Footer, go to privacy, text:privacy">Privacy</a></li>
        <li class="mr-3"><a href="https://github.com/security" data-ga-click="Footer, go to security, text:security">Security</a></li>
        <li class="mr-3"><a href="https://status.github.com/" data-ga-click="Footer, go to status, text:status">Status</a></li>
        <li><a href="https://help.github.com" data-ga-click="Footer, go to help, text:help">Help</a></li>
    </ul>

    <a href="https://github.com" aria-label="Homepage" class="footer-octicon" title="GitHub">
      <svg aria-hidden="true" class="octicon octicon-mark-github" height="24" version="1.1" viewBox="0 0 16 16" width="24"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z"/></svg>
</a>
    <ul class="list-style-none d-flex flex-wrap ">
        <li class="mr-3"><a href="https://github.com/contact" data-ga-click="Footer, go to contact, text:contact">Contact GitHub</a></li>
      <li class="mr-3"><a href="https://developer.github.com" data-ga-click="Footer, go to api, text:api">API</a></li>
      <li class="mr-3"><a href="https://training.github.com" data-ga-click="Footer, go to training, text:training">Training</a></li>
      <li class="mr-3"><a href="https://shop.github.com" data-ga-click="Footer, go to shop, text:shop">Shop</a></li>
        <li class="mr-3"><a href="https://github.com/blog" data-ga-click="Footer, go to blog, text:blog">Blog</a></li>
        <li><a href="https://github.com/about" data-ga-click="Footer, go to about, text:about">About</a></li>

    </ul>
  </div>
</div>



  <div id="ajax-error-message" class="ajax-error-message flash flash-error">
    <svg aria-hidden="true" class="octicon octicon-alert" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M8.865 1.52c-.18-.31-.51-.5-.87-.5s-.69.19-.87.5L.275 13.5c-.18.31-.18.69 0 1 .19.31.52.5.87.5h13.7c.36 0 .69-.19.86-.5.17-.31.18-.69.01-1L8.865 1.52zM8.995 13h-2v-2h2v2zm0-3h-2V6h2v4z"/></svg>
    <button type="button" class="flash-close js-ajax-error-dismiss" aria-label="Dismiss error">
      <svg aria-hidden="true" class="octicon octicon-x" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M7.48 8l3.75 3.75-1.48 1.48L6 9.48l-3.75 3.75-1.48-1.48L4.52 8 .77 4.25l1.48-1.48L6 6.52l3.75-3.75 1.48 1.48z"/></svg>
    </button>
    You can't perform that action at this time.
  </div>


    <script crossorigin="anonymous" src="https://assets-cdn.github.com/assets/compat-137851324479.js"></script>
    <script crossorigin="anonymous" src="https://assets-cdn.github.com/assets/frameworks-ad86efb96de1.js"></script>
    
    <script async="async" crossorigin="anonymous" src="https://assets-cdn.github.com/assets/github-53d5033e4efa.js"></script>
    
    
    
    
  <div class="js-stale-session-flash stale-session-flash flash flash-warn flash-banner d-none">
    <svg aria-hidden="true" class="octicon octicon-alert" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M8.865 1.52c-.18-.31-.51-.5-.87-.5s-.69.19-.87.5L.275 13.5c-.18.31-.18.69 0 1 .19.31.52.5.87.5h13.7c.36 0 .69-.19.86-.5.17-.31.18-.69.01-1L8.865 1.52zM8.995 13h-2v-2h2v2zm0-3h-2V6h2v4z"/></svg>
    <span class="signed-in-tab-flash">You signed in with another tab or window. <a href="">Reload</a> to refresh your session.</span>
    <span class="signed-out-tab-flash">You signed out in another tab or window. <a href="">Reload</a> to refresh your session.</span>
  </div>
  <div class="facebox" id="facebox" style="display:none;">
  <div class="facebox-popup">
    <div class="facebox-content" role="dialog" aria-labelledby="facebox-header" aria-describedby="facebox-description">
    </div>
    <button type="button" class="facebox-close js-facebox-close" aria-label="Close modal">
      <svg aria-hidden="true" class="octicon octicon-x" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M7.48 8l3.75 3.75-1.48 1.48L6 9.48l-3.75 3.75-1.48-1.48L4.52 8 .77 4.25l1.48-1.48L6 6.52l3.75-3.75 1.48 1.48z"/></svg>
    </button>
  </div>
</div>


  </body>
</html>

