// ageinfo: calculate age info of each dept


typedef struct
{
	// while processing departments and members
	int         currdept;      // current department (-1 before we start)

	// results:
	int         ndepts;        // number of depts
	deptnamestr *deptname;	   // dynarray deptname[ndepts]
	int         *agesum;	   // dynarray agesum[ndepts]
	int         *nmembers;	   // dynarray nmembers[ndepts]
	double      *avgage;	   // dynarray avgage[ndepts]
} ageinfo;


extern void calc_ageinfo( list dl, ageinfo * a );
extern void free_ageinfo( ageinfo * a );
