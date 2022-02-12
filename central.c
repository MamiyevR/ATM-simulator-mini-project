#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>
#include <sys/types.h>
#include <unistd.h>
#include "sort.h"

void merge(FILE *fp1, FILE *fp2)
{
  FILE *fp = fopen("transMerged.txt", "w");
  fseek(fp1, 0, SEEK_SET);
  fseek(fp2, 0, SEEK_SET);
  char line[30];
  while (fgets(line, 30, fp1))
  {
    fprintf(fp, "%s", line);
  }
  while (fgets(line, 30, fp2))
  {
    fprintf(fp, "%s", line);
  }
  fclose(fp);
}

void updateBalance(int newBal, char *strBal)
{
  strBal[0] = newBal > 0 ? '+' : '-';
  char tmp[17];
  newBal = abs(newBal);
  sprintf(tmp, "%d", newBal);
  int i = 1;
  int j = strlen(tmp);
  while (i < 16)
  {
    if (16 - j > i)
    {
      strBal[i] = '0';
    }
    else
    {
      strBal[i] = tmp[i + j - 16];
    }
    i++;
  }
  strBal[16] = '\0';
}

void update(FILE *master, FILE *trans)
{
  FILE *fp = fopen("updateMaster.txt", "w");
  fseek(trans, 0, SEEK_SET);
  fseek(master, 0, SEEK_SET);
  char l1[59];
  char line[30];
  while (fgets(l1, 59, master))
  {
    fprintf(fp, "%s", l1);
  }
  fclose(fp);
  FILE *updFile = fopen("updateMaster.txt", "r+");
  while (fgets(line, 30, trans))
  {
    fseek(updFile, 0, SEEK_SET);
    char tmpAccNum[17];
    memcpy(tmpAccNum, &line[0], 16);
    tmpAccNum[16] = '\0';
    char line2[59];
    while (fgets(line2, 59, updFile))
    {
      char tmpAccNum2[17];
      memcpy(tmpAccNum2, &line2[20], 16);
      tmpAccNum2[16] = '\0';
      if (strcmp(tmpAccNum, tmpAccNum2) == 0)
      {
        char operation[2];
        char amount[8];
        char balance[17];
        memcpy(operation, &line[16], 1);
        operation[1] = '\0';
        memcpy(amount, &line[17], 7);
        amount[7] = '\0';
        memcpy(balance, &line2[42], 16);
        balance[16] = '\0';
        char *tmp;
        long intAmount = strtol(amount, &tmp, 10);
        long intBalance = strtol(balance, &tmp, 10);
        fseek(updFile, -58, SEEK_CUR);
        if (strcmp(operation, "D") == 0)
        {
          int newBal = intBalance + intAmount;
          char strBal[17];
          updateBalance(newBal, strBal);
          memcpy(&line2[42], strBal, 16);
          fprintf(fp, "%s", line2);
        }
        else
        {
          int newBal = intBalance - intAmount;
          char strBal[17];
          updateBalance(newBal, strBal);
          memcpy(&line2[42], strBal, 16);
          fprintf(fp, "%s", line2);
        }
        break;
      }
    }
  }
  fclose(updFile);
}

void negReport(FILE *fp)
{
  FILE *report = fopen("negReport.txt", "w");
  fseek(fp, 0, SEEK_SET);
  char line[60];
  while (fgets(line, 60, fp))
  {
    char balance[17];
    memcpy(balance, &line[42], 16);
    balance[16] = '\0';
    if (balance[0] == '-')
    {
      char buff[87];
      char name[21];
      char accNum[17];
      memcpy(name, &line[0], 20);
      name[20] = '\0';
      memcpy(&buff[0], "Name: ", 6);
      memcpy(&buff[6], name, 20);
      memcpy(&buff[26], " Account Number: ", 17);
      memcpy(accNum, &line[20], 16);
      accNum[16] = '\0';
      memcpy(&buff[43], accNum, 16);
      memcpy(&buff[59], " Balance: ", 10);
      memcpy(&buff[69], balance, 16);
      buff[86] = '\0';
      fprintf(report, "%s\n", buff);
    }
  }
  fclose(report);
}

int main(void)
{
  FILE *master = fopen("master.txt", "r");
  if (master == NULL)
  {
    printf("Master.txt is non-existing file!\n");
    exit(1);
  }

  sort_transaction("trans711.txt", "transSorted711.txt");
  sort_transaction("trans713.txt", "transSorted713.txt");

  FILE *trans711 = fopen("trans711.txt", "r");
  FILE *trans713 = fopen("trans713.txt", "r");
  merge(trans711, trans713);
  sort_transaction("transMerged.txt", "transSorted.txt");

  FILE *transSorted = fopen("transSorted.txt", "r");
  update(master, transSorted);

  FILE *updateMaster = fopen("updateMaster.txt", "r");
  negReport(updateMaster);

  fclose(master);
  fclose(trans711);
  fclose(trans713);
  fclose(transSorted);
  fclose(updateMaster);
  return 0;
}