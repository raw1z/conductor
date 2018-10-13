app_name = "conductor"
bucket_name = "#{app_name}.raw1.fr"
bucket_uri = "s3://#{bucket_name}"
bucket_path = "conf/s3/#{bucket_name}"
aws_region = `aws configure get region`
endpoint = "http://#{bucket_name}.s3-website-#{aws_region}.amazonaws.com"

namespace :deploy do
  task :init do
    sh %{
    aws s3 mb #{bucket_uri}
    }
    puts "Website endpoint is: #{endpoint}"
  end

  task :build do
    sh %{
    elm make src/Main.elm --optimize --output=pomodoro.js
    rm -rf deploy
    parcel build index.html --out-dir deploy
    }
  end

  task :run => :build do
    sh %{
      aws s3 sync deploy/ #{bucket_uri} --acl public-read
    }
  end
end
