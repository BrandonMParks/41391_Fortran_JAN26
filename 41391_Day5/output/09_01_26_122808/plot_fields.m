% Auto-generated MATLAB plotting script
% Loads fields from fields_data.m in the same folder and animates a patch-style plot
thisDir = fileparts(mfilename('fullpath'));
run(fullfile(thisDir,'fields_data.m'));
nSteps = numel(fields);
if nSteps == 0
  error('No fields found in fields_data.m');
end
vmin = inf; vmax = -inf;
for kk = 1:nSteps
  f = fields{kk};
  vmin = min(vmin, min(f(:)));
  vmax = max(vmax, max(f(:)));
end
if ~exist('times','var')
  times = 1:nSteps;
end
gifFile = fullfile(thisDir,'time_temp.gif');
if exist(gifFile,'file')
  delete(gifFile);
end
delayTime = 0.10;
fontSize = 18;
figure(1);
for k = 1:nSteps
  field = fields{k};
  clf;
  h = pcolor(field);
  set(h, 'EdgeColor', 'none');
  axis equal tight;
  cb = colorbar;
  cb.Label.String = 'Temperature, T';
  cb.FontSize = fontSize;
  cb.Label.FontSize = fontSize;
  caxis([vmin vmax]);
  title(sprintf('t = %g [s]', times(k)));
  set(gca,'FontSize',fontSize);
  set(get(gca,'Title'),'FontSize',fontSize);
  drawnow;
  fr = getframe(gcf);
  im = frame2im(fr);
  [A,map] = rgb2ind(im,256);
  if k == 1
    imwrite(A,map,gifFile,'gif','LoopCount',inf,'DelayTime',delayTime);
  else
    imwrite(A,map,gifFile,'gif','WriteMode','append','DelayTime',delayTime);
  end
end

% Final state as a surface plot
finalField = fields{end};
figure(2); clf;
hs = surf(finalField);
zlabel('Temperature, T','FontSize',fontSize)
shading interp;
cb2 = colorbar;
cb2.Label.String = 'Temperature, T';
cb2.FontSize = fontSize;
cb2.Label.FontSize = fontSize;
caxis([vmin vmax]);
axis tight;
view(45,30);
title(sprintf('Final Temperature (t = %g [s])', times(end)));
set(gca,'FontSize',fontSize);
set(get(gca,'Title'),'FontSize',fontSize);
set(hs, 'EdgeColor', 'k');
pngFile = fullfile(thisDir,'final_temp.png');
print(gcf, pngFile, '-dpng', '-r150');
