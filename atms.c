#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>
#include <sys/types.h>
#include <unistd.h>

bool getAccount(FILE *fp, char *accountNum, char *password, char *balance)
{
  char line[59];
  bool isMatch = false;
  fseek(fp, 0, SEEK_SET);
  while (fgets(line, 59, fp))
  {
    char tmpAccNum[17];
    char tmpPassword[7];
    memcpy(tmpAccNum, &line[20], 16);
    tmpAccNum[16] = '\0';
    if (strcmp(tmpAccNum, accountNum) == 0)
    {
      memcpy(tmpPassword, &line[36], 6);
      tmpPassword[6] = '\0';
      if (strcmp(tmpPassword, password) == 0)
      {
        memcpy(balance, &line[42], 16);
        balance[16] = '\0';
        isMatch = true;
        break;
      }
      else
      {
        break;
      }
    }
  }
  return isMatch;
}

void recordTime(int timeStamp, char *recordTime)
{
  char strTime[6];
  sprintf(strTime, "%d", timeStamp);
  int i = 0;
  int j = strlen(strTime);
  while (i < 5)
  {
    if (5 - j > i)
    {
      recordTime[i] = '0';
    }
    else
    {
      recordTime[i] = strTime[i + j - 5];
    }
    i++;
  }
  recordTime[5] = '\0';
}

void transAmount(double amount, char *recordAmount)
{
  int i = 0;
  char tmp[7];
  amount *= 100;
  sprintf(tmp, "%.0f", amount);
  int j = strlen(tmp);
  while (i < 7)
  {
    if (7 - j > i)
    {
      recordAmount[i] = '0';
    }
    else
    {
      recordAmount[i] = tmp[i + j - 7];
    }
    i++;
  }
  recordAmount[7] = '\0';
}

int main()
{
  bool cont = true;
  int timeStamp = 0;
  FILE *fp = fopen("master.txt", "r");
  if (fp == NULL)
  {
    printf("Master.txt is non-existing file!\n");
    exit(1);
  }
  FILE *fp1 = fopen("trans711.txt", "w");
  if (fp1 == NULL)
  {
    printf("Error in opening trans711.txt\n");
    exit(1);
  }
  FILE *fp2 = fopen("trans713.txt", "w");
  if (fp2 == NULL)
  {
    printf("Error in opening trans713.txt\n");
    exit(1);
  }

  printf("##############################################\n");
  printf("##         Gringotts Wizarding Bank         ##\n");
  printf("##                 Welcome                  ##\n");
  printf("##############################################\n");
  while (cont)
  {
    char recordT[6];
    recordTime(timeStamp, recordT);
    char atmNum[10];
    char accountNum[50];
    char password[50];
    char balance[50];
    char service[10];
    char deposit[10];
    char withdrawal[10];
    char transferAccount[50];
    char transferAmount[10];
    char continued[10];
    char recordAmount[8];
    double amount;
    double intBalance;
    memset(atmNum, 0, sizeof(atmNum));
    memset(accountNum, 0, sizeof(accountNum));
    memset(password, 0, sizeof(password));
    memset(balance, 0, sizeof(balance));
    memset(service, 0, sizeof(service));
    memset(deposit, 0, sizeof(deposit));
    memset(withdrawal, 0, sizeof(withdrawal));
    memset(transferAccount, 0, sizeof(transferAccount));
    memset(transferAmount, 0, sizeof(transferAmount));
    printf("=> PLEASE CHOOSE THE ATM\n");
    printf("=> PRESS 1 FOR ATM 711\n");
    printf("=> PRESS 2 FOR ATM 713\n");
    if (fgets(atmNum, sizeof(atmNum), stdin))
    {
      atmNum[strcspn(atmNum, "\n")] = 0;
    }
    else
    {
      printf("=> INVALID INPUT\n");
      memset(atmNum, 0, sizeof(atmNum));
      continue;
    }
    if (strcmp(atmNum, "1") != 0 && strcmp(atmNum, "2") != 0)
    {
      printf("=> INVALID INPUT\n");
      continue;
    }

    FILE *fileNum;
    if (atmNum[0] == '1')
    {
      fileNum = fp1;
    }
    else
    {
      fileNum = fp2;
    }

    bool validAccount = false;
    while (!validAccount)
    {
      printf("=> ACCOUNT\n");
      if (fgets(accountNum, sizeof(accountNum), stdin))
      {
        accountNum[strcspn(accountNum, "\n")] = 0;
      }
      else
      {
        printf("=> INVALID INPUT\n");
        memset(accountNum, 0, sizeof(accountNum));
        continue;
      }
      printf("=> PASSWORD\n");
      if (fgets(password, sizeof(password), stdin))
      {
        password[strcspn(password, "\n")] = 0;
      }
      else
      {
        printf("=> INVALID INPUT\n");
        memset(password, 0, sizeof(password));
        continue;
      }
      bool x = getAccount(fp, accountNum, password, balance);
      if (!x)
      {
        printf("=> INCORRECT ACCOUNT/PASSWORD\n");
        memset(accountNum, 0, sizeof(accountNum));
        memset(password, 0, sizeof(password));
        continue;
      }
      else
      {
        validAccount = true;
      }
    }
    char *tmp;
    long x = strtol(balance, &tmp, 10);
    intBalance = x / 100.00;
    if (intBalance < 0)
    {
      printf("=> NEGATIVE REMAINS TRANSACTION ABORT\n");
      continue;
    }
    bool validService = false;
    while (!validService)
    {
      printf("=> PLEASE CHOOSE YOUR SERVICE\n");
      printf("=> PRESS D FOR DEPOSIT\n");
      printf("=> PRESS W FOR WITHDRAWAL\n");
      printf("=> PRESS T FOR TRANSFER\n");
      if (fgets(service, sizeof(service), stdin))
      {
        service[strcspn(service, "\n")] = 0;
      }
      else
      {
        printf("=> INVALID INPUT\n");
        memset(service, 0, sizeof(service));
        continue;
      }

      if (strcmp(service, "D") != 0 &&
          strcmp(service, "W") != 0 &&
          strcmp(service, "T") != 0 &&
          strcmp(service, "d") != 0 &&
          strcmp(service, "w") != 0 &&
          strcmp(service, "t") != 0)
      {
        printf("=> INVALID INPUT\n");
        memset(service, 0, sizeof(service));
        continue;
      }
      else
      {
        validService = true;
      }
    }
    if (strcmp(service, "D") == 0 || strcmp(service, "d") == 0)
    {
      while (1)
      {
        printf("=> AMOUNT\n");
        if (fgets(deposit, sizeof(deposit), stdin))
        {
          deposit[strcspn(deposit, "\n")] = 0;
        }
        else
        {
          printf("=> INVALID INPUT\n");
          memset(deposit, 0, sizeof(deposit));
          continue;
        }

        if (deposit[0] == '-')
        {
          printf("=> INVALID INPUT\n");
          memset(deposit, 0, sizeof(deposit));
          continue;
        }
        else
        {
          char *tmp;
          amount = strtod(deposit, &tmp);
          transAmount(amount, recordAmount);
          fprintf(fileNum, "%s%c%s%s\n", accountNum, 'D', recordAmount, recordT);
          break;
        }
      }
    }
    else if (strcmp(service, "W") == 0 || strcmp(service, "w") == 0)
    {
      while (1)
      {
        printf("=> AMOUNT\n");
        if (fgets(withdrawal, sizeof(withdrawal), stdin))
        {
          withdrawal[strcspn(withdrawal, "\n")] = 0;
        }
        else
        {
          printf("=> INVALID INPUT\n");
          memset(withdrawal, 0, sizeof(withdrawal));
          continue;
        }

        if (withdrawal[0] == '-')
        {
          printf("=> INVALID INPUT\n");
          memset(withdrawal, 0, sizeof(withdrawal));
          continue;
        }
        else
        {
          char *tmp;
          amount = strtod(withdrawal, &tmp);
          if (amount > intBalance)
          {
            printf("=> INSUFFICIENT BALANCE\n");
            memset(withdrawal, 0, sizeof(withdrawal));
            continue;
          }
          else
          {
            transAmount(amount, recordAmount);
            fprintf(fileNum, "%s%c%s%s\n", accountNum, 'W', recordAmount, recordT);
            break;
          }
        }
      }
    }
    else
    {
      while (1)
      {
        printf("=> TARGET ACCOUNT\n");
        if (fgets(transferAccount, sizeof(transferAccount), stdin))
        {
          transferAccount[strcspn(transferAccount, "\n")] = 0;
        }
        else
        {
          printf("=> INVALID INPUT\n");
          memset(transferAccount, 0, sizeof(transferAccount));
          continue;
        }
        if (strcmp(transferAccount, accountNum) == 0)
        {
          printf("=> YOU CANNOT TRANSFER TO YOURSELF\n");
          memset(transferAccount, 0, sizeof(transferAccount));
          continue;
        }
        char line[59];
        fseek(fp, 0, SEEK_SET);
        bool isExist = false;
        while (fgets(line, 59, fp))
        {
          char tmpAccNum[17];
          memcpy(tmpAccNum, &line[20], 16);
          tmpAccNum[16] = '\0';
          if (strcmp(tmpAccNum, transferAccount) == 0)
          {
            isExist = true;
            break;
          }
        }
        if (!isExist)
        {
          printf("=> TARGET ACCOUNT DOES NOT EXIST\n");
          memset(transferAccount, 0, sizeof(transferAccount));
          continue;
        }
        else
        {
          break;
        }
      }
      while (1)
      {
        printf("=> AMOUNT\n");
        if (fgets(transferAmount, sizeof(transferAmount), stdin))
        {
          transferAmount[strcspn(transferAmount, "\n")] = 0;
        }
        else
        {
          printf("=> INVALID INPUT\n");
          memset(transferAmount, 0, sizeof(transferAmount));
          continue;
        }

        if (transferAmount[0] == '-')
        {
          printf("=> INVALID INPUT\n");
          memset(transferAmount, 0, sizeof(transferAmount));
          continue;
        }
        else
        {
          char *tmp;
          amount = strtod(transferAmount, &tmp);
          if (amount > intBalance)
          {
            printf("=> INSUFFICIENT BALANCE\n");
            memset(transferAmount, 0, sizeof(transferAmount));
            continue;
          }
          else
          {
            transAmount(amount, recordAmount);
            fprintf(fileNum, "%s%c%s%s\n", accountNum, 'W', recordAmount, recordT);
            timeStamp++;
            recordTime(timeStamp, recordT);
            fprintf(fileNum, "%s%c%s%s\n", transferAccount, 'D', recordAmount, recordT);
            break;
          }
        }
      }
    }
    while (1)
    {
      printf("=> CONTINUE?\n");
      printf("=> N FOR NO\n");
      printf("=> Y FOR YES\n");
      if (fgets(continued, sizeof(continued), stdin))
      {
        continued[strcspn(continued, "\n")] = 0;
      }
      else
      {
        printf("=> INVALID INPUT\n");
        memset(continued, 0, sizeof(continued));
        continue;
      }

      if (strcmp(continued, "Y") != 0 &&
          strcmp(continued, "y") != 0 &&
          strcmp(continued, "N") != 0 &&
          strcmp(continued, "n") != 0)
      {
        printf("=> INVALID INPUT\n");
        memset(service, 0, sizeof(service));
        continue;
      }

      else
      {
        if (strcmp(continued, "N") == 0 || strcmp(continued, "n") == 0)
        {
          cont = false;
        }
        timeStamp++;
        break;
      }
    }
  }
  fclose(fp);
  fclose(fp1);
  fclose(fp2);
  return 0;
}