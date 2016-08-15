---
title: "Dutch National Flag Problem"
author: Austin
---

Just saw this problem on Glassdoor, given to someone interviewing for an Internship position with Facebook:

> Given an array of randomly arranged lower case letters, uppercase letters, and numbers, sort the array such that all lower case letters come before all uppercase letters, which come before all numbers. The classes of characters do not need to be in order in their respective sections.

So basically, convert ``` Z a 6 t 2 1 T ``` to ``` a t Z T 6 2 1 ```, rearrange it so that the lowercase letters are on the left, uppercase letters in the middle, and numbers on the right.



``` java
void dutchFlag(List<Object> list) {
  int low = 0, equal = 0;
  int high = list.size() - 1;
  while (equal <= high) {
    Object o = list.get(equal);
    if (o instanceof Integer) {
      equal++;
    } else if (o instanceof Character) {
      Character char = (Character)o;
      if (Character.isLowerCase(char)) {
        swap(list, low, equal);
        low++;
        equal++;
      } else if (Character.isUpperCase(char)) {
        swap(list, high, equal);
        high--;
      }
    }
  }
}

void swap(List<Object> list, int i, int j) {
  Object temp = list.get(i);
  list.set(i, list.get(j));
  list.set(j, temp);
}
```
