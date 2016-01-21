package cz.cvut.kbss.jopa.example04.service;

import cz.cvut.kbss.jopa.example04.model.Student;
import cz.cvut.kbss.jopa.example04.persistence.dao.StudentDao;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

@Service
public class StudentRepositoryService {

    @Autowired
    private StudentDao studentDao;

    public List<Student> findAll() {
        return studentDao.findAll();
    }

    public Student findByKey(String key) {
        if (key == null || key.isEmpty()) {
            return null;
        }
        return studentDao.findByKey(key);
    }

    public void persist(Student student) {
        Objects.requireNonNull(student);
        student.setKey(Long.toString(System.currentTimeMillis()));
        student.generateUri();
        studentDao.persist(student);
    }

    public void delete(Student student) {
        Objects.requireNonNull(student);
        studentDao.delete(student);
    }
}
