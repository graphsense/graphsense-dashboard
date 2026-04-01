#!/usr/bin/env node

/**
 * CommonJS version of codegen.sh
 * Generates Elm code from Figma design files using elm-codegen
 */

const { readFileSync, writeFileSync, existsSync } = require('fs');
const { join, dirname } = require('path');
const { run } = require('elm-codegen');
const yargs = require('yargs');
const { hideBin } = require('yargs/helpers');

// __filename and __dirname are available as globals in CommonJS

// Parse command line arguments using yargs
const argv = yargs(hideBin(process.argv))
    .option('file-id', {
        alias: 'f',
        describe: 'Figma file ID (required for --refresh)',
        type: 'string'
    })
    .option('api-token', {
        alias: 'a',
        describe: 'Figma API token (required for --refresh)',
        type: 'string'
    })
    .option('whitelist', {
        alias: 'w',
        describe: 'Comma-separated list of frames to whitelist (use quotes for spaces)',
        type: 'string',
        coerce: (arg) => {
            if (!arg) return [];
            // Handle both quoted and unquoted comma-separated values
            return arg.split(',').map(item => item.trim().replace(/^['"]|['"]$/g, '')).filter(item => item.length > 0);
        }
    })
    .option('components', {
        alias: 'c',
        describe: 'Comma-separated list of components to whitelist (use quotes for spaces)',
        type: 'string',
        coerce: (arg) => {
            if (!arg) return [];
            // Handle both quoted and unquoted comma-separated values
            return arg.split(',').map(item => item.trim().replace(/^['"]|['"]$/g, '')).filter(item => item.length > 0);
        }
    })
    .option('refresh', {
        describe: 'Refresh figma.json from Figma API',
        type: 'boolean'
    })
    .option('plugin', {
        alias: 'p',
        describe: 'Generate code for a specific plugin',
        type: 'string'
    })
    .option('debug', {
        describe: 'Enable debug output',
        type: 'boolean'
    })
    .help('help', 'Show this help message')
    .alias('help', 'h')
    .parseSync();

const figmaFileId = argv['file-id'];
const figmaApiToken = argv['api-token'];
const figmaWhitelistFrames = argv.whitelist || [];
const figmaWhitelistComponents = argv.components || [];
const refresh = argv.refresh || false;
const pluginName = argv.plugin;
const debug = argv.debug || false;

async function main() {
    try {
        if (refresh) {
            await handleRefreshMode();
        } else {
            await handleGenerateMode();
        }
    } catch (error) {
        console.error('Error:', error.message);
        process.exit(1);
    }
}

async function handleRefreshMode() {
    if (!figmaFileId) {
        console.error('-f <figma file id> required');
        process.exit(1);
    }

    if (!figmaApiToken) {
        console.error('-a <figma api token> required');
        process.exit(1);
    }

    console.log(`Refreshing figma file from ${figmaFileId}...`);

    let flags, outputDir;
    if (!pluginName) {
        // Core refresh mode
        flags = {
            figma_file: figmaFileId,
            api_key: figmaApiToken
        };
        outputDir = join(__dirname, '..', 'theme');
    } else {
        // Plugin refresh mode
        const pluginFigmaPath = join(__dirname, '..', 'plugins', pluginName, 'theme', 'figma.json');
        if (!existsSync(pluginFigmaPath)) {
            console.log(`No ${pluginFigmaPath} found to refresh. Exiting.`);
            process.exit(0);
        }

        let whitelist = null;
        const whitelistPath = join(__dirname, '..', 'plugins', pluginName, 'theme', 'whitelist.json');
        if (existsSync(whitelistPath)) {
            try {
                whitelist = JSON.parse(readFileSync(whitelistPath, 'utf8'));
            } catch (e) {
                console.error('Error parsing whitelist.json:', e.message);
                process.exit(1);
            }
        }

        flags = {
            plugin_name: pluginName,
            figma_file: figmaFileId,
            api_key: figmaApiToken,
            whitelist: whitelist
        };
        outputDir = join(__dirname, '..', 'plugins', pluginName, 'theme');
    }

    await runCodegen(pluginName, flags, outputDir);
}

async function handleGenerateMode() {
    console.log('Running codegen...');

    let flags, outputDir;
    if (!pluginName) {
        // Core generate mode
        const figmaJsonPath = join(__dirname, '..', 'theme', 'figma.json');
        if (!existsSync(figmaJsonPath)) {
            console.error(`No figma.json found at ${figmaJsonPath}`);
            process.exit(1);
        }

        const figmaContent = JSON.parse(readFileSync(figmaJsonPath, 'utf8'));
        
        flags = {
            whitelist: {
                frames: figmaWhitelistFrames,
                components: figmaWhitelistComponents
            },
            theme: figmaContent
        };
        outputDir = join(__dirname, '..', 'generated', 'theme');
    } else {
        // Plugin generate mode
        const pluginFigmaPath = join(__dirname, '..', 'plugins', pluginName, 'theme', 'figma.json');
        if (!existsSync(pluginFigmaPath)) {
            console.log(`No ${pluginFigmaPath} found. Skipping.`);
            process.exit(0);
        }

        const colormapsPath = join(__dirname, '..', 'generated', 'theme', 'colormaps.json');
        if (!existsSync(colormapsPath)) {
            console.warn('colormaps.json not found. You need to run core generation first.');
            console.warn('Run: npm run codegen (without --plugin flag)');
            process.exit(1);
        }

        const colormaps = JSON.parse(readFileSync(colormapsPath, 'utf8'));
        const pluginFigmaContent = JSON.parse(readFileSync(pluginFigmaPath, 'utf8'));
        
        flags = {
            colormaps: colormaps,
            theme: pluginFigmaContent,
            whitelist: {frames:[], components:[]}
        };
        outputDir = join(__dirname, '..', 'generated', 'theme');
    }

    await runCodegenIteratively(flags, outputDir);
}

async function runCodegenIteratively(flags, outputDir) {
  if(!flags.theme) {
    console.error('no theme found!')
    process.exit(1)
  }

  const nodes = {...flags.theme.figma.nodes}
  const colorNodes = {}
  for (const node in nodes) {
    if(!nodes[node].document.name.startsWith('Colors ')) continue
    colorNodes[node] = nodes[node]
  }
  for (const node in nodes) {
    const name = nodes[node].document.name
    if(flags.whitelist.frames.length && flags.whitelist.frames.indexOf(name) === -1) {
      continue
    }
    flags.theme.figma.nodes = {...colorNodes}
    flags.theme.figma.nodes[node] = nodes[node]
    await runCodegen(name, flags, outputDir)
  }
}

async function runCodegen(name, flags, outputDir) {
    const elmFile = join(__dirname, '..', 'codegen', 'Generate.elm');
    
    const options = {
        debug: debug,
        output: outputDir,
        flags: flags,
        cwd: join(__dirname, '..', 'codegen')
    };

    console.log(`Running elm-codegen for ${name}...`);

    const origLog = console.log
    const origWarn = console.log
    const origErr = console.error
    let success = ""
    let logs = ""
    console.log = (str) => {
        if (str.indexOf(" files generated in ") !== -1) {
          success = str
        }
        logs += str
      }
    console.warn = () => {}
    let errors = ""
    console.error = (str) => {errors += str}

    // Temporarily override process.exit to prevent elm-codegen from exiting
    const originalProcessExit = process.exit;
    process.exit = () => {};

    await run(elmFile, options);


    // Restore original process.exit
    process.exit = originalProcessExit;
    console.log = origLog
    console.warn = origWarn
    console.error = origErr

    if (success) {
      console.log('Code generation completed successfully!');
      console.log(success)
    } else {
      console.error('Code generation completed with errors:');
      console.error(logs)
      console.error(errors)
      const out_file = '/tmp/codegen_options_' + Date.now() + '.json'
      writeFileSync(out_file, JSON.stringify(options, null, 2))
      console.error('Input options written to ' + out_file)
      process.exit(1);
    }
}

main();
