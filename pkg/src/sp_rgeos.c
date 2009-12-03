#include "rgeos.h"
/*	Modified 24 May 2005 Roger S. Bivand for maptools
	Written by Joseph O'Rourke
	orourke@cs.smith.edu
	October 27, 1995

	Computes the centroid (center of gravity) of an arbitrary
	simple polygon via a weighted sum of signed triangle areas,
	weighted by the centroid of each triangle.
	Reads x,y coordinates from stdin.  
	NB: Assumes points are entered in ccw order!  
	E.g., input for square:
		0	0
		10	0
		10	10
		0	10
	This solves Exercise 12, p.47, of my text,
	Computational Geometry in C.  See the book for an explanation
	of why this works. Follow links from
		http://cs.smith.edu/~orourke/

*/


#define DIM     2               /* Dimension of points */
typedef double  tPointd[DIM];   /* type double point */


double  Area2( tPointd a, tPointd b, tPointd c );
void    FindCG( int n, tPointd *P, tPointd CG, double *Areasum2 );
void    Centroid3( tPointd p1, tPointd p2, tPointd p3, tPointd c );

void	rgeos_spRFindCG( int *n, double *x, double *y, double *area ) {

	int i, nn;
	tPointd *P;
	tPointd CG;
	double Areasum2;
	nn = n[0];
	P = (tPointd *) R_alloc(nn, sizeof(tPointd));
	for (i=0; i<nn; i++) {
		P[i][0] = x[i];
		P[i][1] = y[i];
	}
	FindCG(nn, P, CG, &Areasum2);
	area[0] = CG[0];
	area[1] = CG[1];
	area[2] = Areasum2/2;
	return;
}

/*      
        Returns the cg in CG.  Computes the weighted sum of
	each triangle's area times its centroid.  Twice area
	and three times centroid is used to avoid division
	until the last moment.
*/
void     FindCG( int n, tPointd *P, tPointd CG, double *Areasum2)
{
        int     i;
        double  A2;        /* Partial area sum */    
	tPointd Cent3;

	CG[0] = 0;
	CG[1] = 0;
        Areasum2[0] = 0;
	for (i = 1; i < n-1; i++) {
	        Centroid3( P[0], P[i], P[i+1], Cent3 );
	        A2 =  Area2( P[0], P[i], P[i+1]);
		CG[0] += A2 * Cent3[0];
		CG[1] += A2 * Cent3[1];
		Areasum2[0] += A2;
	      }
        CG[0] /= 3 * Areasum2[0];
        CG[1] /= 3 * Areasum2[0];
	return;
}
/*
	Returns three times the centroid.  The factor of 3 is
	left in to permit division to be avoided until later.
*/
void    Centroid3( tPointd p1, tPointd p2, tPointd p3, tPointd c )
{
        c[0] = p1[0] + p2[0] + p3[0];
        c[1] = p1[1] + p2[1] + p3[1];
	return;
}
/* 
        Returns twice the signed area of the triangle determined by a,b,c,
        positive if a,b,c are oriented ccw, and negative if cw.
*/
double     Area2( tPointd a, tPointd b, tPointd c )
{
	double area;
	area = (b[0] - a[0]) * (c[1] - a[1]) - (c[0] - a[0]) * (b[1] - a[1]);
	return(area);
}

