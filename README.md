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

For further details read the attached Manual or visit the website of the SO SR
https://slovak.statistics.sk/wps/portal/ext/products/software.tools

You are also free to try some calibration with the test data. Its usage
is described in Chapter 6 of the Manual.
