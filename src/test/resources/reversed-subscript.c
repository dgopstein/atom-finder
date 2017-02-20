void nested_vectors_with_constant_indices() {
   int vec[3] = {1, 2, 3};
   int vec_in_vec[3][3] = {{1,2,3},{4,5,6},{7,8,9}};
   int vec_in_vec2[3][3][2] = {{{1,2},{3,4},{5,6}},{{7,8},{9,10},{11,12}},{{13,14},{15,16},{17,18}}};

   printf("%d\n", vec[1]);
   printf("%d\n", 1[vec]);

   printf("%d\n", vec_in_vec[1][2]);
   printf("%d\n", 1[vec_in_vec[2]]);
   printf("%d\n", 1[2[vec_in_vec]]);
   printf("%d\n", 2[vec_in_vec][1]);

   printf("%d\n", vec_in_vec2[1][2][3]);
   printf("%d\n", 3[vec_in_vec2[1][2]]);
   printf("%d\n", 3[2[vec_in_vec2[1]]]);
   printf("%d\n", 3[2[1[vec_in_vec2]]]);
}

void nested_vectors_with_int_variable_indices() {
   int vec_in_vec[3][3] = {{1,2,3},{4,5,6},{7,8,9}};

   int i = 1, j = 2;

   printf("%d\n", vec_in_vec[1][2]);
   printf("%d\n", vec_in_vec[i][j]);
   printf("%d\n", j[vec_in_vec[i]]);
   printf("%d\n", j[i[vec_in_vec]]);
}

void nested_vectors_with_int_expression_indices() {
   int vec_in_vec[3][3] = {{1,2,3},{4,5,6},{7,8,9}};

   int i = 1, j = 2;

   printf("%d\n", vec_in_vec[0+1][3-1]);
   printf("%d\n", vec_in_vec[0+i][4-j]);
   printf("%d\n", (3-1)[vec_in_vec[0+1]]);
   printf("%d\n", (4-j)[(0+i)[vec_in_vec]]);

   printf("%d\n",  j[vec_in_vec[i^i]]);
   printf("%d\n",  (j||i)[vec_in_vec[i & i]]);

   printf("%d\n", vec_in_vec[i][((i+2)*0)*32598 + (j/(i + i + i))*0 + j]);
   printf("%d\n", (((i+2)*0)*32598 + (j/(i + i + i))*0 + j)[vec_in_vec[i]]);
}

void nested_vectors_with_pre_increment_indicies() {
   int vec_in_vec[3][3] = {{1,2,3},{4,5,6},{7,8,9}};

   int i = 1, j = 2;

   printf("%d\n", vec_in_vec[++i][--j]);
   printf("%d\n", (--j)[vec_in_vec[++i]]);
   printf("%d\n", (++j)[(--i)[vec_in_vec]]);
}

void strings_with_constant_indices() {
   printf("%c\n", "vec"[1]);
   printf("%c\n", 1["vec"]);
  
   int i = 2;

   printf("%c\n", "vec"[--i]);
   printf("%c\n", (--i)["vec"]);
}

void pointer_arithmetic(){
   int vec_in_vec[3][3] = {{1,2,3},{4,5,6},{7,8,9}};

   printf("%d\n", (vec_in_vec+1)[0][0]);
   printf("%d\n", 0[0[vec_in_vec+1]]);
   printf("%d\n", (vec_in_vec-0+1)[0][0]);
   printf("%d\n", 0[0[vec_in_vec-0+1]]);
   printf("%d\n", (0+vec_in_vec+1)[0][0]);
   printf("%d\n", 0[0[0+vec_in_vec+1]]);

   printf("%c\n", ("vec"+1)[0]);
   printf("%c\n", (0)["vec"+1]);
   printf("%c\n", ("vec"-0+1)[0]);
   printf("%c\n", (0)["vec"-0+1]);
   printf("%c\n", (0+"vec"+1)[0]);
   printf("%c\n", (0)[0+"vec"+1]);
}

int main() {
  nested_vectors_with_constant_indices();
  nested_vectors_with_int_variable_indices();
  nested_vectors_with_int_expression_indices();
  nested_vectors_with_pre_increment_indicies();
  strings_with_constant_indices();
  pointer_arithmetic();
}

// simplified from: linux/drivers/md/md.c
static blk_qc_t md_make_request(struct request_queue *q, struct bio *bio)
{
	const int rw = bio_data_dir(bio);
	unsigned int sectors;
	part_stat_add(cpu, &mddev->gendisk->part0, sectors[rw], sectors); // probably not an atom
}
