# Calif 4.0

This is a Shiny web app for calibration of weights of statistical surveys
prepared by Statistical Office of the Slovak Republic.

To launch Calif run this command in R:
```r
shiny::runGitHub('Calif', 'SO-SR', destdir = getwd(), launch.browser = TRUE)
```

To install required packages run in R:
```r
install.packages(c('shiny', 'sampling', 'nleqslv', 'haven'))
```

To download single file app visit the [webpage of the SO SR](https://slovak.statistics.sk/wps/portal/ext/products/software.tools/!ut/p/z1/jZDNDoIwEISfxSfoIH_luBgsNZVQpIC9GE6GRNGD8fkl6NXC3Db7zWxmmWUds2P_Hq79a3iM_W2azza6NKrkaeoRkB05iJKQG1N4IQ9YOwM6ll-AF1UGWVMpqkPgIQiZndaktT6ppoFotntI3xMojAHq6Od3AHbN_Z2gPIgVwJUIISk3VaJ9H-Sv8-OPCOv8DsC641tmZ8TVYCnDLj35eTeTOgySNh-yLq3D/dz/d5/L0lDUmlTUSEhL3dHa0FKRnNBLzROV3FpQSEhL2Vu/)
where you can find the Calif 4.0.R code that can be sourced in R and run by **calif()** command.

Calif runs in your web browser, in order to guarantee a proper functioning make sure you are 
using the latest version of the browser. Once Calif is launched, the whole session is 
undertaken locally within R (in the background) and no data is sent outside your PC 
(a web browser just serves for displaying the GUI).

You are also free to try some calibration with the test data. Its usage
is described in Chapter 6 of the Manual.

Copyright (C) 2014-2018 The Statistical Office of the Slovak Republic

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Calif makes use of R packages shiny, sampling, nleqslv and haven.
Small changes were made to the calib function of the sampling package and 
to the pie function of the graphics package.
