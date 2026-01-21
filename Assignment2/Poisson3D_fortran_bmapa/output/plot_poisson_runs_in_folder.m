% plot_poisson_runs_in_folder.m
%
% Loads all .mat files in the same folder as this script and plots one
% curve per run (threads vs GS wall time).
%
% Expected format in each .mat:
%   run.threads
%   run.wall_time_s

scriptDir = fileparts(mfilename('fullpath'));
listing = dir(fullfile(scriptDir, '*.mat'));

if isempty(listing)
    error('No .mat files found in: %s', scriptDir);
end

figure;
hold on;

fs = 20; lw = 2; ms = 10;

plottedCount = 0;

for k = 1:numel(listing)
    matPath = fullfile(scriptDir, listing(k).name);
    S = load(matPath);

    if ~isfield(S, 'run')
        continue;
    end

    run = S.run;

    if ~isfield(run, 'threads') || ~isfield(run, 'wall_time_s')
        continue;
    end

    if isfield(run, 'label') && ~isempty(run.label)
        label = run.label;
    else
        [~, label, ~] = fileparts(listing(k).name);
    end

    plot(run.threads, run.wall_time_s, '-o', 'LineWidth', lw, 'MarkerSize', ms, ...
         'DisplayName', label);
    plottedCount = plottedCount + 1;
end

if plottedCount == 0
    error('No usable run structs found in any .mat in: %s', scriptDir);
end

xlabel('Recruited worker thread count, $n_{threads}$ [1]','Interpreter','latex');
% time label
% ylabel('Algorithm parallel section wall time, $t_{par}$ [s]','Interpreter','latex');
% speed-up label
ylabel('Parallelized speed-up vs. Sequential, $\frac{t_{par}}{t_{seq}}$ [1]','Interpreter','latex');
grid on;
legend('Location',  'best');
% title('Iterative linear solver parallelization speed-ups');

% Manual plot adjustments
ax = gca;
ax.FontSize = fs;
ax.FontName = 'Latex';
% ax.XScale = 'log';
hold off;

clear S run matPath listing k plottedCount label scriptDir
