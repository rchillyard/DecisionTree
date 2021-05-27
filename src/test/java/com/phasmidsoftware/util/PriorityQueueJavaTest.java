package com.phasmidsoftware.util;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;

import static org.junit.Assert.assertEquals;

public class PriorityQueueJavaTest {

    @Test
    public void testPoll() {
        Collection<Integer> list = new ArrayList<>();
        list.add(10);
        list.add(5);
        list.add(8);
        list.add(0);
        list.add(1);
        PriorityQueueJava<Integer> pq = new PriorityQueueJava<>(list);
        assertEquals(0, Objects.requireNonNull(pq.poll()).intValue());
        assertEquals(1, Objects.requireNonNull(pq.poll()).intValue());
        assertEquals(5, Objects.requireNonNull(pq.poll()).intValue());
        assertEquals(8, Objects.requireNonNull(pq.poll()).intValue());
        assertEquals(10, Objects.requireNonNull(pq.poll()).intValue());
    }
}