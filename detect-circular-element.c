/*
 * Detect an element in a linked list that introduces a cycle into the
 * list.  Requires that pointers be word-aligned on the platform in
 * question.
 *
 * 2013 / Julian Squires <julian@cipht.net>
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

struct node {
    struct node *next;
    /* arbitrary value payload here */
    int id;
};


static struct node *identify_backreference(struct node *head)
{
    struct node *p = head, *q;

    while (p->next) {
        q = p->next;
        p->next = (struct node *)((intptr_t)p->next | 1);
        if ((intptr_t)q->next&1)
            goto cleanup;
        p = q;
    }
    return NULL;

cleanup:
    while (head != p->next) {
        head->next = (struct node *)((intptr_t)head->next & ~1);
        head = head->next;
    }
    return p;
}


int main(int argc, char **argv)
{
    enum { N = 20 };
    struct node list[N], *head = list;

    for (int i = 0; i < N; ++i)
        list[i] = (struct node){.next = &list[i+1], .id = i};
    list[N-1].next = NULL;

    srand(time(NULL));
    int traitor = 1 + rand() % (N-1), victim = rand() % traitor;
    printf("Introducing a backreference at %d, pointing to %d\n", traitor, victim);
    list[traitor].next = &list[victim];

    struct node *result = identify_backreference(head);
    if (result)
        printf("Element introducing circularity is %d\n", result->id);
    else
        printf("No circularity detected.\n");

    return EXIT_SUCCESS;
}
