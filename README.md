# dispatch_RA_model
UC/ED model, toy case, and post-processing scripts.

1. Input data for PJM cases must be downloaded separately. 
2. Some input files are larger and cannot integrate with Github.

Also, as of 8/21/2019, the default version of the model implements primary synchronized reserves as a hard constraint. In cases with ORDC, this should be modified to a penalty factor integrated into the objective function, as is true for other reserve products. However, this implementation better approximates historic PJM practice.
