import java.io.IOException;

import org.jdom2.JDOMException;

import edu.stanford.services.explorecourses.Course;
import edu.stanford.services.explorecourses.Department;
import edu.stanford.services.explorecourses.School;
import edu.stanford.services.explorecourses.ExploreCoursesConnection;
import edu.stanford.services.explorecourses.Section;
import edu.stanford.services.explorecourses.Instructor;
import edu.stanford.services.explorecourses.MeetingSchedule;



import java.io.PrintWriter;
import java.io.File;
import java.io.*;
import java.util.*;

/** Prints out information for all courses offered at Stanford in an Academic Year **/
public class CourseInformation
{
  public static void main(String[] args) throws IOException, JDOMException
  {
  	PrintWriter writer=new PrintWriter("/home/meric/Documents/CS221/Project/courseinfo.txt");
    ExploreCoursesConnection connection = new ExploreCoursesConnection();
    for(School s : connection.getSchools())
      for(Department d : s.getDepartments())
        for(Course c : connection.getCoursesByQuery(d.getCode())){
          Set<String> allinstructors=new HashSet<String>();
          String instructors="";
          for (Section section: c.getSections()){
            for(MeetingSchedule schedule : section.getMeetingSchedules()){
                for(Instructor inst: schedule.getInstructors()){
                    allinstructors.add(inst.getFirstName()+" "+inst.getLastName());
                }
            }
          }
          Object[] allinst=allinstructors.toArray();
          for (int i=0; i<allinst.length; i++)
            instructors=instructors+ allinst[i].toString()+" ";
        	writer.println("Course Title: "+c.getTitle());//get title of course
        	writer.println("Course Instructors: "+ instructors); //to be filled in here
        	writer.println("Course Units: "+Integer.toString(c.getMinimumUnits())+"-"+Integer.toString(c.getMaximumUnits()));
        	writer.println("Course Description: "+c.getDescription());
        	writer.println("----------------------------");	
      }
        
    // for (Course c: connection.getCoursesByQuery("Programming Abstractions")){
    // 	for (Section s: c.getSections()){
    // 		// for(Instructor inst: s.getInstructors()){ //ERROR HERE!!
    // 		// 	//System.out.println(inst.getLocation());
    // 		// }
    //       System.out.println(s.getSectionNumber());
    // 	}
    // }
    // 	//System.out.println(c.getDescription());
    writer.close();
  }
} 
