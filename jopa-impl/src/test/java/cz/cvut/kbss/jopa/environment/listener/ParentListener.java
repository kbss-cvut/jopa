package cz.cvut.kbss.jopa.environment.listener;

import cz.cvut.kbss.jopa.model.annotations.PostPersist;
import cz.cvut.kbss.jopa.model.annotations.PrePersist;

import java.lang.reflect.Method;

public class ParentListener {

    @PrePersist
    public void prePersist(Object instance) {
        System.out.println("PrePersist");
    }

    @PostPersist
    private void postPersist(Object instance) {
        System.out.println("Private postPersist");
    }

    public static Method getPrePersistMethod() throws Exception {
        return ParentListener.class.getDeclaredMethod("prePersist", Object.class);
    }

    public static Method getPostPersistMethod() throws Exception {
        return ParentListener.class.getDeclaredMethod("postPersist", Object.class);
    }
}
