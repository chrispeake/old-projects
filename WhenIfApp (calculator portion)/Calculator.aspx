<%@ Page Title="Calculator" Language="C#" MasterPageFile="~/Site.Master" AutoEventWireup="true" CodeBehind="Calculator.aspx.cs" Inherits="WebApplication1.Calculator" %>

<asp:Content ID="BodyContent" ContentPlaceHolderID="MainContent" runat="server">

    <div class="jumbotron">
        <section id="calculator-app">
            <div id="calc-tabs">
                TABS
                <ul>
                  <li><a href="DegreeProgress">Degree Progress</a></li>
                  <li><a href="Calculator">Calculator</a></li>
                  <li><a href="ClassSearch">ClassSearch</a></li>
                </ul>
            </div>
             <div id="calc-title">
                Calculator

            </div>
            <div id="calc-results">
                    <table >
      <tr>
        <td>
          <asp:Button Text="Search" BorderStyle="None" ID="SearchTab" CssClass="Initial" runat="server"
              OnClick="SearchTab_Click" />
          <asp:Button Text="Results" BorderStyle="None" ID="ResultTab" CssClass="Initial" runat="server"
              OnClick="ResultTab_Click" />
          <asp:MultiView ID="MainView" runat="server">
            <asp:View ID="SearchView" runat="server">
              <table style="width: 100%; border-width: 1px; border-color: #666; border-style: solid">
                <tr>
                  <td>
                    <h3>
                    </h3>
                <div>
                    <label>Degree:</label>
                    <asp:DropDownList ID="ddlDegree" runat="server" Width="370px" AppendDataBoundItems="true" OnSelectedIndexChanged="ddlDegree_SelectedIndexChanged" AutoPostBack="true">
                        <asp:ListItem Text="Select Degree" Value="-1"></asp:ListItem>
                    </asp:DropDownList>
                </div>
                <div>
                    <label>Option:</label>
                    <asp:DropDownList ID="ddlOption" runat="server" Width="370px" AppendDataBoundItems="true" AutoPostBack="true" Enabled = "false">
                        <asp:ListItem Text="  -  " Value="-1"></asp:ListItem>
                    </asp:DropDownList>
                </div>
                <div>
                <div><label>Select number of classes per:</label></div>
                    Quarter:
                    <asp:DropDownList ID="ddlQtrN" runat="server">
                        <asp:ListItem Text="#" Value="-1"></asp:ListItem>
                        <asp:ListItem Text="1" Value="1"></asp:ListItem>
                        <asp:ListItem Text="2" Value="2"></asp:ListItem>
                        <asp:ListItem Text="3" Value="3"></asp:ListItem>
                        <asp:ListItem Text="4" Value="4"></asp:ListItem>
                        <asp:ListItem Text="5" Value="5"></asp:ListItem>
                        <asp:ListItem Text="6" Value="6"></asp:ListItem>
                        <asp:ListItem Text="7" Value="7"></asp:ListItem>
                    </asp:DropDownList>
                    Summer:
                      
                    <asp:DropDownList ID="ddlSummerN" runat="server">
                        <asp:ListItem Text="#" Value="-1"></asp:ListItem>
                        <asp:ListItem Text="0" Value="0"></asp:ListItem>
                        <asp:ListItem Text="1" Value="1"></asp:ListItem>
                        <asp:ListItem Text="2" Value="2"></asp:ListItem>
                        <asp:ListItem Text="3" Value="3"></asp:ListItem>
                        <asp:ListItem Text="4" Value="4"></asp:ListItem>
                        <asp:ListItem Text="5" Value="5"></asp:ListItem>
                        <asp:ListItem Text="6" Value="6"></asp:ListItem>
                        <asp:ListItem Text="7" Value="7"></asp:ListItem>
                    </asp:DropDownList>
                    <p></p>
                    Starting Quarter:
                    <asp:DropDownList ID="ddlQtrStart" runat="server">
                        <asp:ListItem Text="-" Value="-1"></asp:ListItem>
                        <asp:ListItem Text="Fall" Value="5"></asp:ListItem>
                        <asp:ListItem Text="Winter" Value="1"></asp:ListItem>
                        <asp:ListItem Text="Spring" Value="2"></asp:ListItem>
                        <asp:ListItem Text="Summer" Value="3"></asp:ListItem>
                    </asp:DropDownList>
                    Starting Year:
                    <asp:DropDownList ID="ddlYearStart" runat="server">
                        <asp:ListItem Text="-" Value="-1"></asp:ListItem>
                        <asp:ListItem Text="2014" Value="2014"></asp:ListItem>
                        <asp:ListItem Text="2015" Value="2015"></asp:ListItem>
                        <asp:ListItem Text="2016" Value="2016"></asp:ListItem>
                        <asp:ListItem Text="2017" Value="2017"></asp:ListItem>
                        <asp:ListItem Text="2018" Value="2018"></asp:ListItem>
                        <asp:ListItem Text="2019" Value="2019"></asp:ListItem>
                        <asp:ListItem Text="2020" Value="2020"></asp:ListItem>
                        <asp:ListItem Text="2021" Value="2021"></asp:ListItem>
                        <asp:ListItem Text="2022" Value="2022"></asp:ListItem>
                        <asp:ListItem Text="2023" Value="2023"></asp:ListItem>
                    </asp:DropDownList>
                    <br />
                    <asp:Button ID="searchBtn" runat="server" Text="   Search   " OnClick="RunSearch_Click" BackColor="#70C600" CssClass="btn-submit" Font-Bold="True" Font-Size="Small" style=" left:50%;"/>
                    <asp:Literal runat="server" ID="ErrorMessage"  />
                </div>
                    
                  </td>
                </tr>
              </table>
            </asp:View>
            <asp:View ID="ResultView" runat="server">
              <table style="height: 100%; width: 100%; border-width: 1px; border-color: #666; border-style: solid">
                <tr>
                  <td>
                    <h3>
                    </h3>
                      <p>--------------------------------- Results --------------------------------- </p>
                      <asp:ListBox name="resultBox" ID="resultsBox" runat="server" CssClass="resultBox" style=" height: 280px; width: 100%;  margin: auto;" >
                      </asp:ListBox>
                  </td>
                </tr>
              </table>
            </asp:View>
          </asp:MultiView>
        </td>
      </tr>
    </table>

            </div>
        </section>
    </div>


</asp:Content>
