// Preston Doman
// CS300
// Frupal Project
// This program is a microservice that compares the coordinates passed as
// arguments to the coordinate/item pairs in the data file generated
// by the coordinator service. It returns the name of the item at the coordinate
// in the data file preceded by 'ok' or 'error' followed by an explanation upon
// encountering an error, as expected by the coordinator.
// Run by typing ./main lookup x y

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char* argv[])
{
	char	data_file[] = "data";
	FILE*	fp;
	int	x_file, y_file, x_arg, y_arg;
	char	line[100];
	char	item_name[100];
	char	default_item[100];
	bool	found = false;

	if (argc != 4)
	{
		fprintf(stderr, "error Required format: ./service lookup x y\n");
		exit(EXIT_FAILURE);
	}

	/* get coordinates to check from command line */
	// argv[1] is assumed to be "lookup"
	x_arg = atoi(argv[2]);	
	y_arg = atoi(argv[3]);

	fp = fopen(data_file, "r");
	if (!fp)
	{
		fprintf(stderr, "error File %s could not be opened.\n", data_file);		
		exit(EXIT_FAILURE);
	}

	/* compare coordinates from arguments to those in the data file */
	fgets(default_item, sizeof(default_item), fp);	
	while (!found && fgets(line, sizeof(line), fp))
	{
		// [^\n] grabs everything between the last int and line end
		sscanf(line, "%d %d %[^\n]", &x_file, &y_file, item_name);
		if (x_arg == x_file && y_arg == y_file)
		{
			found = true;
		}
	}

	if (found)
	{
		printf("ok %s\n", item_name);
	}
	else
	{
		printf("ok %s", default_item);
	}

	fclose(fp);

	return 0;
}
