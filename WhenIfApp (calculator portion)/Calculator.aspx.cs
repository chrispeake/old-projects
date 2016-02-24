using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Data;
using System.Data.SqlClient;

namespace WebApplication1
{
    public partial class Calculator : Page
    {
        static Boolean reload = true;
        SqlConnection MyConnection = new SqlConnection("Data Source=cpeake.asuscomm.com;Integrated Security=False;User ID=matthew;Password=matthew;Connect Timeout=15;Encrypt=False;TrustServerCertificate=False;ApplicationIntent=ReadWrite;MultiSubnetFailover=False; Initial Catalog=WhenIf_Data;");
        DataTable degreeTable = new DataTable();
        DataTable optionTable = new DataTable();

        string userName = "";
        string depaulID = "";

        //global variables that store degree data to minimize queries

        // ReqIDNumberNeeded <ReqID, #> => stores # of classes needed to take for given ReqID key
        Dictionary<int, int> ReqIDNumberNeeded = new Dictionary<int, int>();

        // ReqIDClassesNeeded <ReqID, list<classes>> => stores list of classes that fill given ReqID key
        Dictionary<int, List<string>> ReqIDClassesNeeded = new Dictionary<int, List<string>>();

        // Stores ReqIDs flagged as intro (must be taken before others)
        List<int> introReqID = new List<int>();
        // Stores all other ReqIDs
        List<int> mainReqID = new List<int>();

        //bool to store if intro courses need to be taken first
        bool introNeeded = true;

        List<string> studentHistory = new List<string>();

        List<int> date = new List<int>() { 2014, 1 };
        int QtrN = 1;
        int SummerN = 0;

        protected void Page_Load(object sender, EventArgs e)
        {
            //switch to searchTab
            SearchTab.CssClass = "Clicked";
            ResultTab.CssClass = "Initial";
            //SavedTab.CssClass = "Initial";
            MainView.ActiveViewIndex = 0;

            if (IsPostBack)
            {
                reload = false;
            }
            else
            {
                reload = true;
            }
            if (!User.Identity.IsAuthenticated) // if the user is already logged in
            {
                Response.Redirect("~/Account/Login", false);
            }

            if (reload)
            {
                using (MyConnection)
                {
                    try
                    {
                        SqlDataAdapter adapter = new SqlDataAdapter("SELECT DegreeID, DegreeDescription FROM wi.tbl_degree", MyConnection);
                        adapter.Fill(degreeTable);

                        ddlDegree.DataSource = degreeTable;
                        ddlDegree.DataTextField = "DegreeDescription";
                        ddlDegree.DataValueField = "DegreeID";
                        ddlDegree.DataBind();
                        adapter.Dispose();
                    }
                    catch (Exception ex)
                    {
                        // Handle the error
                        throw ex;
                    }
                }
            }

        }

        //loads ddlOption items once ddlDegree is chosen
        protected void ddlDegree_SelectedIndexChanged(object sender, EventArgs e)
        {
            //A postback occurred - use the selection to bind the values to your other drop-down

            ddlOption.ClearSelection();
            ddlOption.Items.Clear();
            ddlOption.Items.Add(new ListItem("-", "-1"));
            using (MyConnection)
            {
                try
                {
                    SqlDataAdapter adapter = new SqlDataAdapter("SELECT D1.OptionID, OptionDescription " +
                                                                    "FROM wi.tbl_deg_opt_relationship D1 INNER JOIN wi.tbl_option D2 " +
                                                                        "ON D1.OptionID = D2.OptionID " +
                                                                    "WHERE DegreeID = " + ddlDegree.SelectedValue, MyConnection);
                    optionTable.Clear();
                    adapter.Fill(optionTable);
                    ddlOption.DataSource = optionTable;
                    ddlOption.DataTextField = "OptionDescription";
                    ddlOption.DataValueField = "OptionID";
                    ddlOption.DataBind();

                    if (ddlOption.Items.Count > 1)
                    {
                        ddlOption.Enabled = true;

                    }
                    else
                    {
                        ddlOption.Enabled = false;
                    }
                }
                catch (Exception ex)
                {
                    // Handle the error
                    throw ex;
                }

            }
        }

        protected void SearchTab_Click(object sender, EventArgs e)
        {
            SearchTab.CssClass = "Clicked";
            ResultTab.CssClass = "Initial";
            //SavedTab.CssClass = "Initial";
            MainView.ActiveViewIndex = 0;
        }

        protected void ResultTab_Click(object sender, EventArgs e)
        {
            SearchTab.CssClass = "Initial";
            ResultTab.CssClass = "Clicked";
            //SavedTab.CssClass = "Initial";
            MainView.ActiveViewIndex = 1;
        }

        protected void SavedTab_Click(object sender, EventArgs e)
        {
            SearchTab.CssClass = "Initial";
            ResultTab.CssClass = "Initial";
            //SavedTab.CssClass = "Clicked";
            MainView.ActiveViewIndex = 2;
        }


        protected void Button1_Click(object sender, EventArgs e)
        {
            Response.Redirect("~/Account/Register", false);
        }

        protected void Menu1_MenuItemClick(object sender, MenuEventArgs e)
        {

        }

        //handles search click, checks for form completion, fills in initial data, and runs search
        protected void RunSearch_Click(object sender, EventArgs e)
        {
            //validates dropdownlist selections are all chosen
            if (ddlDegree.SelectedValue == "-1")
            {
                ErrorMessage.Text = "Error: Please Select a Degree.";
            }
            else if (ddlOption.SelectedValue == "-1")
            {
                ErrorMessage.Text = "Error: Please Select a Degree Option.";
            }
            else if (ddlQtrN.SelectedValue == "-1")
            {
                ErrorMessage.Text = "Error: Please select the number of classes you wish to take per quarter.";
            }
            else if (ddlSummerN.SelectedValue == "-1")
            {
                ErrorMessage.Text = "Error: Please select the number of classes you wish to take per summer quarter.";
            }
            else if (ddlQtrStart.SelectedValue == "-1")
            {
                ErrorMessage.Text = "Error: Please select your starting quarter.";
            }
            else if (ddlYearStart.SelectedValue == "-1")
            {
                ErrorMessage.Text = "Error: Please select your starting quarter.";
            }
            else
            {
                ErrorMessage.Visible = false;
                resultsBox.Items.Clear();
                fillSearchInfo();
                fillStudentInfo();
                fillDegreeInfo();
                MergeStudentDegreeInfo();
                RunSearch();

                //switch to result view
                SearchTab.CssClass = "Initial";
                ResultTab.CssClass = "Clicked";
                //SavedTab.CssClass = "Initial";
                MainView.ActiveViewIndex = 1;
            }

        }

        //removes degree requirements already fullfilled in studentHistory
        protected void MergeStudentDegreeInfo()
        {    
            foreach(string cl in studentHistory)
            {
                bool UnusedCl = true;
                foreach (int ReqID in introReqID)
                {
                    if (ReqIDNumberNeeded[ReqID]>0 && ReqIDClassesNeeded[ReqID].Contains(cl))
                    {
                        UnusedCl = false;
                        ReqIDClassesNeeded[ReqID].Remove(cl);
                        ReqIDNumberNeeded[ReqID] = ReqIDNumberNeeded[ReqID] - 1;

                        if (ReqIDNumberNeeded[ReqID] < 1) introNeeded = false;
                    }
                }
                foreach (int ReqID in mainReqID)
                {
                    if (UnusedCl && ReqIDNumberNeeded[ReqID] > 0 && ReqIDClassesNeeded[ReqID].Contains(cl))
                    {
                        UnusedCl = false;
                        ReqIDClassesNeeded[ReqID].Remove(cl);
                        ReqIDNumberNeeded[ReqID] = ReqIDNumberNeeded[ReqID] - 1;
                    }
                }
            }
        }

        // increments global var date by quarter
        protected void incrementDate()
        {
            if (date[1] >= 5)
            {
                date[0] = date[0] + 1;
                date[1] = 1;
            }
            else date[1] = date[1] + 1;
        }

        // increments global var date by quarter, skips summer sessions
        protected void incrementDateNoSummer()
        {
            if (date[1] >= 5)
            {
                date[0] = date[0] + 1;
                date[1] = 1;
            }
            else if (date[1] == 2 || date[1] == 3 || date[1] == 4)
            {
                date[1] = 5;
            }
            else date[1] = date[1] + 1;
        }

        //fills selected values needed for search from dropdownlists
        protected void fillSearchInfo()
        {
            QtrN = Convert.ToInt32(ddlQtrN.SelectedValue);
            SummerN = Convert.ToInt32(ddlSummerN.SelectedValue);
            date[0] = Convert.ToInt32(ddlYearStart.SelectedValue);
            date[1] = Convert.ToInt32(ddlQtrStart.SelectedValue);
        }
        //fills student history from server using username
        protected void fillStudentInfo()
        {
            studentHistory = new List<string>();
            SqlDataReader classRdr;
            SqlDataReader rdr;
            userName = User.Identity.Name;
            depaulID = "";

            string queryGetDepaulId = "SELECT DEPAULID " +
                                             "FROM dbo.AspNetUsers " +
                                             "WHERE UserName = @USER_NAME";


            SqlCommand readCmd = new SqlCommand(queryGetDepaulId, MyConnection);
            readCmd.Parameters.Add("@USER_NAME", System.Data.SqlDbType.VarChar);
            readCmd.Parameters["@USER_NAME"].Value = userName;
            MyConnection.Open();


            rdr = readCmd.ExecuteReader();

            while (rdr.Read())
            {
                depaulID = rdr.GetString(0);
            }

            rdr.Close();
            MyConnection.Close();

            string queryGetSavedDegree = "SELECT * " +
                    "FROM dbo.UserDegree " +
                    "WHERE DEPAULID = @DEPAULID";


            SqlCommand readClassCmd = new SqlCommand(queryGetSavedDegree, MyConnection);
            readClassCmd.Parameters.Add("@DEPAULID", System.Data.SqlDbType.VarChar);
            readClassCmd.Parameters["@DEPAULID"].Value = depaulID;
            MyConnection.Open();
            classRdr = readClassCmd.ExecuteReader();

            while (classRdr.Read())
            {
                studentHistory.Add(classRdr.GetString(2));
            }
            MyConnection.Close();
        }

        //fills in degree info from server, stores data in global vars
        protected void fillDegreeInfo()
        {
            //clear previous data
            ReqIDNumberNeeded = new Dictionary<int, int>();
            ReqIDClassesNeeded = new Dictionary<int, List<string>>();
            introReqID = new List<int>();
            mainReqID = new List<int>();
            introNeeded = false;

            SqlDataReader rdr;

            //queries tbl_opt_req_relationship for all reqIDs for selected degree option
            string sql = "SELECT ReqID, IsIntroductory FROM wi.tbl_opt_req_relationship " +
                         "WHERE OptionID = " + ddlOption.SelectedValue.ToString();

            MyConnection.Open();
            SqlCommand cmd = new SqlCommand(sql, MyConnection);
            rdr = cmd.ExecuteReader();

            while (rdr.Read())
            {
                int ReqID = Convert.ToInt32(rdr.GetValue(0));
                bool isIntro = Convert.ToBoolean(rdr.GetValue(1));

                if (isIntro)
                {
                    introNeeded = true;
                    introReqID.Add(ReqID);
                } else
                {
                    mainReqID.Add(ReqID);
                }
            }
            rdr.Close();
            MyConnection.Close();

            //queries tbl_requirement number of classes needed for each reqID
            foreach (int ReqID in introReqID.Concat(mainReqID))
            {
                sql = "SELECT NumRequired FROM wi.tbl_requirement " +
                         "WHERE ReqID = @ReqID;";

                cmd = new SqlCommand(sql, MyConnection);
                cmd.Parameters.Add("@ReqID", System.Data.SqlDbType.VarChar);
                cmd.Parameters["@ReqID"].Value = ReqID;
                MyConnection.Open();
                rdr = cmd.ExecuteReader();

                while (rdr.Read())
                {
                    ReqIDNumberNeeded[ReqID] = Convert.ToInt32(rdr.GetValue(0));
                }
                rdr.Close();
                MyConnection.Close();
                //ensures ReqID entries with an empty list exist in ReqIdClassesNeeded
                ReqIDClassesNeeded[ReqID] = new List<string>();
            }

            //queries tbl_req_course_relationship and loads all classes with same ReqID into Dictionary
            foreach (int ReqID in introReqID.Concat(mainReqID))
            {
                sql = "SELECT CourseNumber FROM wi.tbl_req_course_relationship " +
                         "WHERE ReqID = @ReqID;";

                cmd = new SqlCommand(sql, MyConnection);
                cmd.Parameters.Add("@ReqID", System.Data.SqlDbType.VarChar);
                cmd.Parameters["@ReqID"].Value = ReqID;
                MyConnection.Open();
                rdr = cmd.ExecuteReader();

                while (rdr.Read())
                {
                    //adds each class for given ReqID
                    ReqIDClassesNeeded[ReqID].Add(rdr.GetValue(0).ToString());
                }
                rdr.Close();
                MyConnection.Close();
            }
            MyConnection.Close();

            //allows you to make sure classes are actually added
            /*
            foreach (int ReqID in introReqID.Concat(mainReqID))
            {
                resultsBox.Items.Add(new ListItem(ReqID.ToString(), "alal"));
                foreach(string cl in ReqIDClassesNeeded[ReqID])
                {
                    resultsBox.Items.Add(new ListItem(cl, cl));
                }
                
            }
            */
        }

        // checks whether student satisfies prereqs for given CourseNumber
        // string => bool
        protected bool checkClassPrereqs(string CourseNumber)
        {
            List<string> PrereqSetIDs = new List<string>();
            SqlDataReader rdr;

            //gets PrereqSetIDs for CourseNumber from server, puts them in PrereqSetIDs
            string sql = "SELECT PrereqSetID FROM wi.tbl_prereq_set WHERE CourseNumber = @CourseNumber;";
            MyConnection.Open();
            SqlCommand cmd = new SqlCommand(sql, MyConnection);
            cmd.Parameters.Add("@CourseNumber", System.Data.SqlDbType.VarChar);
            cmd.Parameters["@CourseNumber"].Value = CourseNumber;
            rdr = cmd.ExecuteReader();

            while (rdr.Read())
            {
                //PrereqSetIDs.Add(rdr.GetString(0));
                PrereqSetIDs.Add(rdr.GetValue(0).ToString());
            }

            if (cmd != null) cmd.Dispose();
            if (MyConnection != null) MyConnection.Close();
            if (rdr != null) rdr.Dispose();
            MyConnection.Close();

            //for each PrereqSetID, gets its classes from server and checks whether studenthistory contains them
            foreach (string PrereqSetID in PrereqSetIDs)
            {
                List<string> PrereqSet = new List<string>();
                bool setRet = false;

                sql = "SELECT Prereq FROM wi.tbl_prereq WHERE PrereqSetID = @PrereqSetID;";
                MyConnection.Open();
                cmd = new SqlCommand(sql, MyConnection);
                cmd.Parameters.Add("@PrereqSetID", System.Data.SqlDbType.VarChar);
                cmd.Parameters["@PrereqSetID"].Value = PrereqSetID;
                rdr = cmd.ExecuteReader();

                while (rdr.Read())
                {
                    //checks if returned classes are in StudentHistory
                    if (studentHistory.Contains(rdr.GetString(0))) setRet = true;
                }

                if (cmd != null) cmd.Dispose();
                if (MyConnection != null) MyConnection.Close();
                if (rdr != null) rdr.Dispose();
                MyConnection.Close();

                //returns false if the PrereqSet isn't satisfied
                if (!setRet) return false;
            }

            if (cmd != null) cmd.Dispose();
            if (MyConnection != null) MyConnection.Close();
            if (rdr != null) rdr.Dispose();
            MyConnection.Close();

            //returns true if no prereqs are missing for the PrereqSets
            return true;
        }

        // determines which classes in the given ReqID are for the given date, 
        // only adds classes that have met prereqs
        // Int => list<string> 
        protected List<string> ClassesOffered(int ReqID)
        {
            List<string> ret = new List<string>();
            string qtr = "";

            //convert date[1] to quarter string equivalent
            switch (date[1])
            {
                case 1:
                    qtr = "Wintera";
                    break;
                case 2:
                    qtr = "Springa";
                    break;
                case 3:
                    qtr = "Summer1a";
                    break;
                case 4:
                    qtr = "Summer2a";
                    break;
                case 5:
                    qtr = "Falla";
                    break;
            }

            SqlConnection MyConnection = new SqlConnection("Data Source=cpeake.asuscomm.com;Integrated Security=False;User ID=matthew;Password=matthew;Connect Timeout=15;Encrypt=False;TrustServerCertificate=False;ApplicationIntent=ReadWrite;MultiSubnetFailover=False; Initial Catalog=WhenIf_Data;");
            SqlDataReader rdr;

            foreach (string CourseNumber in ReqIDClassesNeeded[ReqID])
            {
                //resultsBox.Items.Add(new ListItem(CourseNumber, CourseNumber));
                //if (CourseNumber == "GAM 690" && date[1]==1) resultsBox.Items.Add(new ListItem("Looking at " + CourseNumber, CourseNumber));
                string sql = "SELECT Winter, Spring, Summer1, Summer2, Fall FROM wi.tbl_course WHERE CourseNumber = @CourseNumber;";
                MyConnection.Open();
                SqlCommand cmd = new SqlCommand(sql, MyConnection);
                cmd.Parameters.Add("@CourseNumber", System.Data.SqlDbType.VarChar);
                cmd.Parameters["@CourseNumber"].Value = CourseNumber;
                rdr = cmd.ExecuteReader();

                //checks if CourseNumber is offered and have prereqs
                while (rdr.Read())
                {
                    //resultsBox.Items.Add(new ListItem(rdr.GetValue(0).ToString(), rdr.GetValue(0).ToString()));
                    //string s = rdr.GetValue(0).ToString();
                    //System.Diagnostics.Debug.WriteLine(s);
                    bool rdrBool = rdr.GetBoolean(date[1] - 1);
                    bool rdrPre = checkClassPrereqs(CourseNumber);
                    //if (CourseNumber == "GAM 690" && date[1] == 1) resultsBox.Items.Add(new ListItem("Looking at " + CourseNumber + rdrBool.ToString() + "prereq: " + rdrPre.ToString(), CourseNumber));
                    if (rdrBool
                        && checkClassPrereqs(CourseNumber))
                        ret.Add(CourseNumber);
                }
                /*
                string sql = "SELECT @qtr FROM wi.tbl_course WHERE CourseNumber = @CourseNumber;";
                MyConnection.Open();
                SqlCommand cmd = new SqlCommand(sql, MyConnection);
                cmd.Parameters.Add("@CourseNumber", System.Data.SqlDbType.VarChar);
                cmd.Parameters["@CourseNumber"].Value = CourseNumber;
                cmd.Parameters.Add("@qtr", System.Data.SqlDbType.VarChar);
                cmd.Parameters["@qtr"].Value = qtr;
                rdr = cmd.ExecuteReader();
                

                //checks if CourseNumber is offered and have prereqs
                while (rdr.Read())
                {
                    resultsBox.Items.Add(new ListItem(rdr.GetValue(0).ToString(), rdr.GetValue(0).ToString()));
                    if (rdr.GetValue(0).ToString() == "1"
                        && checkClassPrereqs(CourseNumber))
                        ret.Add(CourseNumber);
                }
                */

                if (cmd != null) cmd.Dispose();
                if (MyConnection != null) MyConnection.Close();
                if (rdr != null) rdr.Dispose();

                MyConnection.Close();
            }

            return ret;
        }

        //checks if introNeeded flag still required.
        protected bool checkIntro()
        {
            foreach (int ReqID in introReqID)
            {
                if (ReqIDNumberNeeded[ReqID] > 0) return false;
            }
            return true;
        }

        //searches for classes that are available this quarter that student meets requirements for
        protected List<string> QtrClassSearch()
        {
            List<string> currentClasses = new List<string>();
            List<string> needed = new List<string>();
            List<int> reqList = new List<int>();
            int count;

            if (date[1] == 3 || date[1] == 4) count = SummerN;
            else count = QtrN;

            //if you still need intro courses, you can't take any else
            if (introNeeded)
            {
                reqList = new List<int>(introReqID);
                reqList.Sort();
                foreach (int ReqID in reqList)
                {
                    //gets classes offered that have met prereqs
                    needed = ClassesOffered(ReqID);
                    while (count > 0 && needed.Count() > 0 && ReqIDNumberNeeded[ReqID] > 0)
                    {
                        string course = needed[0];
                        needed.Remove(course);
                        ReqIDClassesNeeded[ReqID].Remove(course);



                        if (!studentHistory.Contains(course) && !currentClasses.Contains(course))
                        {
                            ReqIDNumberNeeded[ReqID] = ReqIDNumberNeeded[ReqID] - 1;
                            count = count - 1;
                            currentClasses.Add(course);
                        }
                    }
                }
                if (checkIntro()) introNeeded = false;
            }
            //searches for main courses
            else
            {
                reqList = new List<int>(mainReqID);
                reqList.Sort();
                foreach (int ReqID in reqList)
                {
                    //gets classes offered that have met prereqs
                    needed = ClassesOffered(ReqID);
                    while (count > 0 && needed.Count() > 0 && ReqIDNumberNeeded[ReqID] > 0)
                    {
                        string course = needed[0];
                        needed.Remove(course);
                        ReqIDClassesNeeded[ReqID].Remove(course);

                        //if(course == "GAM 690") resultsBox.Items.Add(new ListItem("Looking at " + course + "in qtrSearch", course));

                        if (!studentHistory.Contains(course) && !currentClasses.Contains(course))
                        {
                            ReqIDNumberNeeded[ReqID] = ReqIDNumberNeeded[ReqID] - 1;
                            count = count - 1;
                            currentClasses.Add(course);
                        }
                    }
                }
            }
            //adds classes to studenthistory
            foreach (string cl in currentClasses) { studentHistory.Add(cl); }
            return currentClasses;
        }

        //checks whether all ReqID numbers have been completed
        protected bool CompletedReqNumbers()
        {
            foreach (int ReqID in introReqID.Concat(mainReqID))
            {
                if (ReqIDNumberNeeded[ReqID] > 0) return false;
            }
            return true;
        }

        protected void RunSearch()
        {
            List<string> qtrClasses = new List<string>();
            int i = 1;
            string heading = "";

            //while you haven't completed all the ReqID required course numbers, run the qtrsearch
            while (!CompletedReqNumbers())
            {
                qtrClasses = QtrClassSearch();
                foreach (string cl in qtrClasses) { studentHistory.Add(cl); }

                if (date[1] == 3 || date[1] == 4) { heading = "(summer)Qtr #" + i.ToString(); }
                else { heading = "Qtr #" + i.ToString(); }
                
                i = i + 1;
                resultsBox.Items.Add(new ListItem(heading, heading));

                foreach (string cl in qtrClasses)
                {
                    resultsBox.Items.Add(new ListItem(cl, cl));
                }

                if (ddlSummerN.SelectedValue == "0")
                    incrementDateNoSummer();
                else
                    incrementDate();
                if (i > 50) break;

            }
        }
    }
}
 