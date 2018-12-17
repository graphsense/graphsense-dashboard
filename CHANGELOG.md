# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Changed
- Complete redesign and reimplementation from scratch on top of Webpack, ES6 and TailwindCSS
- Responsive layout
- Adapt to version 0.4 of Graphsense REST service
- Visualize transaction flow from left to right (indegree to outdegree)
- Boxes instead of bubbles 
- Merge cluster and address graph views
- Integrate data tables in graph view 
- Infinite scrolling on tables (leveraging paging in the backend)
- Remove block view
- Restrict autocomplete results (addresses only)
- Collapse property box if no node is selected

### Added
- Support multiple keyspaces (crypto-currencies)
- Show statistics for each crypto-currency on the landing page
- Search bar on graph view (load more addresses)
- Data tables for incoming/outgoing neighbors of address/cluster
- Show number of neighbors per node and in property box
- Show number of addresses per cluster node
- Node label switch (id, tags, category, ...)
- User-defined notes per node
- Coloring of nodes based on tags, notes and categories
- Loading indicators
- Logs and error messages (collapsable)
- Pan and zoom
- Save and restore the application state to/from file
- Landing page footer and couple of informational static pages 
- Dockerfile for production deployment
