############################################################################
## 
## Copyright © 2017, Eric T. Moore,  mooreet@gmail.com 
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
############################################################################

rebin <- function(nbins){
  
}

column.names <- c("bin_1024","res","bin_256")
row.names <- c(1:1024)
trans_mat <- matrix(ncol = 3,nrow = 1024,dimnames=list(row.names,column.names))
trans_mat[,1] <- 1:1024
trans_mat[,2] <- y_scale*log(beta/trans_mat[,1])
trans_mat[1,3] <- (beta/exp(trans_mat[1,2]/y_scale) )
range.res <- trans_mat[1,2] - trans_mat[1024,2]
res.width <- range.res/256
for(i in 2:1024){
  trans_mat[i,3] <- ((trans_mat[i-1,2]-trans_mat[i,2])/res.width) + trans_mat[i-1,3]
}

