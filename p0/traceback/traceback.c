/** @file traceback.c
 *  @brief The traceback function
 *
 *  The trackback is the core of this file, it also contains several "helper"
 *  functions that will be used in trackback function
 *
 *  @author Dalong Cheng(dalongc)
 *  @bug No known bugs
 */
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include "traceback_internal.h"

/* function prototypes */

/* @brief trace the function call in current stack
 *
 * @param fp The output stream pointer
 * @return void
 */
void traceback(FILE *fp);

/* @brief read register %ebp value
 *
 * @param void
 * @return the address value stored in %ebp
 */
int* read_ebp();

/* @brief given a ebp pointer, figure out the entry address
 *        of current function
 *
 * @param ebp pointer in strack frame of function
 * @return entry address of function
 */
int extract_function_address(int *ebp_ptr);

/* @brief wrapper function of unix error
 *
 * @param msg a remainding messge of which function cause error 
 * @return void
 */
void unix_error(char *msg); 

/* @brief check whether a string is printable, if the string is printable
 *        calculate the string length
 *
 * @param str input string for checking
 * @param str_len  if string is printable, fill in variable, otherwise 0
 * @return 1 for printable, 0 for non-printable
 */
int is_string_printable(char *str, int *str_len);

/* @brief print string to certain output stream, if the string more than 
 *        25, then print the first 25 letters.
 *
 * @param str string need to be printed
 * @param fp pointer to output stream
 * @return void
 */
void print_string(char *str, FILE *fp);

/* @brief given arg type, offset to ebp address, name, this function will 
 *        output the arg value to certain output stream
 *
 * @param type arg types 0 for int, 1 for float etc.
 * @param offset offset to address where store ebp value
 * @param name the name of arg
 * @param ebp_ptr pointer to ebp value 
 * @param fp pointer to output stream
 * @return void
 */
void output_arg_value(int type, int offset, char* name, char* ebp_ptr, FILE *fp);

/* @brief given index in functions table, this function will extract each arg 
 *        information, then use output_arg_value function to print all arg 
 *        values
 *
 * @param ebp_ptr pointer to ebp value, this argument just pass to out_arg_value,
 *        no use in this function  
 * @param func_table_index, the index in functions table
 * @param pointer to output stream
 * @return void
 */
void extract_arg_info(char* ebp_ptr, int func_table_index, FILE *fp); 

/* @brief given a function entry address, check whether this address has record in 
 *        functions table
 *
 * @param address entry address of certain function 
 * @return -1 for not in functions table, otherwise return index in functions table
 */
int is_in_function_tab(int address);

/* impelmentation of each function prototype mentioned above */
void traceback(FILE *fp) {

    int func_table_index = 0;

    int* ebp = (int*)read_ebp();
    int* ebp_ptr = (int*)(*ebp);
    int address = extract_function_address(ebp_ptr);
    ebp_ptr = (int*)*(ebp_ptr);
    address = extract_function_address(ebp_ptr);

    while ((func_table_index = is_in_function_tab(address)) != -1) { 
        fprintf(fp, "Function %s", functions[func_table_index].name);
        /* output arg information */
        extract_arg_info((char*)ebp_ptr, func_table_index, fp);
        fprintf(fp, ", in\n");

        /* update ebp_ptr to previous functions ebp_ptr */
        ebp_ptr = (int*)*(ebp_ptr);
        address = extract_function_address(ebp_ptr);
    }
}

int extract_function_address(int *ebp_ptr) {
    int *return_address_ptr = ebp_ptr + 1; 
    int return_address = *return_address_ptr;
    int *offset_address_ptr = (int*)(return_address - 5 + 1);
    int offset = *offset_address_ptr;   
    int func_address = return_address + offset;
    
    return func_address;
}

void unix_error(char *msg) {
    fprintf(stdout, "%s error: %s\n", msg, strerror(errno));
    exit(1);
}

int is_string_printable(char *str, int *str_len) {
    int i = 0;
    int fd = 0;
    
    if ((fd = open("/tmp/test", O_WRONLY|O_CREAT, S_IWUSR)) < 0) {
        unix_error("open");
    }

    /* use system function write to test wheter member address is valid */
    while (write(fd, str + i, 1) != -1) {
        if (str[i] == '\0')
            break;
      
        if (!isprint(str[i])) 
            return 0;
       i++;
    }
    *str_len = i;
    close(fd);
    return 1;
}

void print_string(char *str, FILE *fp) {
    int str_len = 0;
    int i = 0;
    if (is_string_printable(str, &str_len)) {
        fprintf(fp, "\"");
        if (str_len > 25) {
            for (i = 0; i < 25; i++) 
                fprintf(fp, "%c", str[i]);
            fprintf(fp, "...");
        } else {
            for (i = 0; i < str_len; i++) 
                fprintf(fp, "%c", str[i]);
        }
        fprintf(fp, "\"");
    } else {
        fprintf(fp, "%p", str);
    }
}

void output_arg_value(int type, int offset, char* name, char* ebp_ptr, FILE *fp) {
    int str_address;
    char **str_array;
    char c;
    int i;
    /* each type has according way of output */
    switch (type) {
        case 0:
            c = *(char*)(ebp_ptr + offset);
            if (isprint(c))
                fprintf(fp, "char %s='%c'", name, c);
            else 
                fprintf(fp, "char %s='\\%o'", name, c); 
            break;
        case 1:
            fprintf(fp, "int %s=%d", name, *(int*)(ebp_ptr + offset));
            break;
        case 2:
            fprintf(fp, "float %s=%f", name, *(float*)(ebp_ptr + offset));
            break;
        case 3:
            fprintf(fp, "double %s=%lf", name, *(double*)(ebp_ptr + offset));
            break;
        case 4:
            str_address = *(int*)(ebp_ptr + offset);
            fprintf(fp, "char *%s=", name);
            print_string((char*)str_address, fp);
            break;
        case 5:
            str_array = (char**)*(int*)(ebp_ptr + offset);
            fprintf(fp, "char **%s={", name);
            for (i = 0; i < 4 && str_array[i][0] != '\0'; i++) {
                if (i == 3) {
                    fprintf(fp, ", ...");
                    break;
                }
                if (i == 0) {
                    print_string(str_array[i], fp);
                } else {
                    fprintf(fp, ", ");
                    print_string(str_array[i], fp);
                }
            }
            fprintf(fp, "}"); 
            break;
        case 6:
            fprintf(fp, "void *%s=0v%x", name, *(int*)(ebp_ptr + offset));
            break;
        default:
            fprintf(fp, "UNKNOWN %x", *(int*)(ebp_ptr + offset));
    }
}

void extract_arg_info(char* ebp_ptr, int func_table_index, FILE *fp) {
    int arg_type;
    int arg_offset;
    char *arg_name;
    int i = 0;
    int first_flag = 0;

    fprintf(fp, "(");
    if (strlen(functions[func_table_index].args[i].name) == 0) {
        fprintf(fp, "void");
    } else {
        while (strlen(functions[func_table_index].args[i].name) != 0) {
            if (first_flag == 0) {
                first_flag = 1;
            } else {
                fprintf(fp, ", ");
            }
            arg_type = functions[func_table_index].args[i].type;
            arg_offset = functions[func_table_index].args[i].offset;
            arg_name = (char*)(functions[func_table_index].args[i].name);
            output_arg_value(arg_type, arg_offset, arg_name, ebp_ptr, fp);
            i++; 
        }
    }
    fprintf(fp, ")");
}

int is_in_function_tab(int address) {
    int i = 0;
    int temp;
    while (strlen(functions[i].name) != 0) {
        temp = (int)functions[i].addr;
        if (temp == address) 
            return i;
        i++;
    }
    return -1;
}
