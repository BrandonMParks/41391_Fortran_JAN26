% extract_poisson_sweep_data.m
%
% Run this script to parse an LSF job output file (Output_*.out)
% produced by the Poisson3D OpenMP sweep and define variables in
% the workspace.
%
% Variables defined:
%   source_file   - path to the parsed .out file
%   N             - interior grid size
%   itermax       - maximum iterations
%   tol           - convergence tolerance
%   alg           - iterative algorithm id
%   omp_dynamic   - 'T' or 'F'
%   threads       - vector of thread counts
%   wall_time_s   - vector of GS wall times (seconds), paired with threads
%   run           - struct that bundles all fields above
%   mat_file      - path to the saved .mat file
%
% Usage:
%   - Default: parses newest Output_*.out in the same folder as this script.
%   - Override: set outFile before running this script, e.g.
%       outFile = 'Output_27626038.out';
%       run('extract_poisson_sweep_data.m')

scriptDir = fileparts(mfilename('fullpath'));

if exist('outFile', 'var') == 1 && ~isempty(outFile)
    candidate = outFile;
    if ~isfile(candidate)
        candidate = fullfile(scriptDir, candidate);
    end
    if ~isfile(candidate)
        error('Could not find output file: %s', outFile);
    end
    source_file = candidate;
else
    listing = dir(fullfile(scriptDir, '*.out'));
    if isempty(listing)
        error('No Output_*.out files found in: %s', scriptDir);
    end
    [~, newestIdx] = max([listing.datenum]);
    source_file = fullfile(scriptDir, listing(newestIdx).name);
end

txt = fileread(source_file);

% -----------------------------
% Extract constants (first match)
% -----------------------------
N_tok = regexp(txt, 'N \(interior grid size\)\s*=\s*(\d+)', 'tokens', 'once');
itermax_tok = regexp(txt, 'itermax \(max iterations\)\s*=\s*(\d+)', 'tokens', 'once');
tol_tok = regexp(txt, 'convergence tolerance\s*=\s*([0-9Ee+\-\.]+)', 'tokens', 'once');
alg_tok = regexp(txt, 'Iterative algorithm\s*=\s*(\d+)', 'tokens', 'once');
omp_dyn_tok = regexp(txt, 'OpenMP dynamic threads\s*=\s*([TF])', 'tokens', 'once');

if isempty(N_tok) || isempty(itermax_tok) || isempty(tol_tok) || isempty(alg_tok)
    error('Failed to parse one or more constants from: %s', source_file);
end

N = str2double(N_tok{1});
itermax = str2double(itermax_tok{1});
tol = str2double(tol_tok{1});
alg = str2double(alg_tok{1});
try
    omp_dynamic = omp_dyn_tok{1};
catch
    fprintf("OMP dynamic/static behavior not captured.\n")
end

% ---------------------------------------------
% Extract (threads, GS wall time) pairs per run
% ---------------------------------------------
pairTokens = regexp(txt, [ ...
    '(?s)Running sweep case:\s*threads=(\d+).*?' , ...
    '\s*wall time \(s\)\s*=\s*([0-9Ee+\-\.]+)'], ...
    'tokens');

if isempty(pairTokens)
    error('No (threads, wall time) pairs found in: %s', source_file);
end

threads = cellfun(@(c) str2double(c{1}), pairTokens);
wall_time_s = cellfun(@(c) str2double(c{2}), pairTokens);

% Ensure paired ordering is strictly by thread count
[threads, sortIdx] = sort(threads(:));
wall_time_s = wall_time_s(sortIdx)';

% -----------------------------
% Bundle + save to .mat
% -----------------------------
[outDir, baseName, ~] = fileparts(source_file);
mat_file = fullfile(outDir, [baseName, '.mat']);

run = struct();
run.source_file = source_file;
run.label = baseName;
run.N = N;
run.itermax = itermax;
run.tol = tol;
run.alg = alg;
run.omp_dynamic = omp_dynamic;
run.threads = threads;
run.wall_time_s = wall_time_s;

save(mat_file, 'run');

clear candidate listing newestIdx N_tok itermax_tok tol_tok alg_tok omp_dyn_tok pairTokens sortIdx scriptDir txt outDir baseName
